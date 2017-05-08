{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

module Integrations.Gmail.Http
  ( listMessages
  , listHistory
  , loadMessage
  , loadMessageLabels
  , requestTokens
  , authCodeUrl
  , GmailError(..)
  , History(..)
  , PageToken(..)
  )
where

import           Control.Lens ((&), (.~), (^.), (?~), (%~))
import           Control.Lens.Prism (_Left, _Right)
import           Control.Monad.State (StateT, liftIO, get, put)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (unpack, pack, intercalate, Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.IO (putStrLn)
import           Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import qualified Integrations.Gmail.JSON.GmailTokenInfo as TR
import qualified Integrations.Gmail.JSON.ListHistoryResponse as LH
import qualified Integrations.Gmail.JSON.ListMessagesResponse as LM
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageRef as MR
import qualified Integrations.Gmail.JSON.MessageLabels as ML
import           Integrations.Gmail.JSON.RefreshResponse (RefreshResponse)
import qualified Integrations.Gmail.JSON.RefreshResponse as RR
import           Integrations.Secrets (gmailClientId, gmailClientSecret)
import           Network.Wreq (FormParam(..))
import qualified Network.Wreq as Wreq
import           Prelude hiding (putStrLn, lookup)

data GmailError = GmailError Int String | ParsingError String
  deriving Show

type GmailApiCall a = StateT GmailTokenInfo IO a



loadMessage :: MR.MessageRef -> GmailApiCall (GetMessageResponse M.Message)
loadMessage = loadMessageGeneric Full

loadMessageLabels :: MR.MessageRef -> GmailApiCall (GetMessageResponse ML.Message)
loadMessageLabels = loadMessageGeneric Minimal

type GetMessageResponse a = Either GmailError a
data Format = Full | Minimal

formatParam :: Format -> Text
formatParam Full = "full"
formatParam Minimal = "minimal"

loadMessageGeneric :: FromJSON a => Format -> MR.MessageRef -> GmailApiCall (GetMessageResponse a)
loadMessageGeneric format ref = do
  response <- gmailHttp ("/messages/" <> MR._id ref) [("format", formatParam format)]
  return $ jsonDecode response id

type ListMessagesResponse = ([MR.MessageRef], Maybe PageToken)

listMessages :: Maybe PageToken -> GmailApiCall (Either GmailError ListMessagesResponse)
listMessages maybeToken = do
  response <- gmailHttp "/messages" params
  return $ jsonDecode response (\m -> (LM._messages m, PageToken <$> LM._nextPageToken m))
  where params = paramsFromPageToken maybeToken

data History
  = MessageAdded MR.MessageRef
  | MessageDeleted MR.MessageRef
  | LabelsAdded MR.MessageRef [Text]
  | LabelsRemoved MR.MessageRef [Text]

type ListHistoryResponse = ([History], Maybe PageToken)

listHistory :: Text -> Maybe PageToken -> GmailApiCall (Either GmailError ListHistoryResponse)
listHistory startHistoryId maybeToken = do
  response <- gmailHttp "/history" params
  return $ jsonDecode response processResponse
  where params = paramsFromPageToken maybeToken ++ [("startHistoryId", startHistoryId)]
        processResponse json = ( mconcat $ extractFromHistoryItem <$> LH._history json
                               , PageToken <$> LH._nextPageToken json)
        extractLabelsAdded item = LabelsAdded (LH._message item) (LH._labelIds item)
        extractLabelsRemoved item = LabelsRemoved (LH._message item) (LH._labelIds item)
        extractFromHistoryItem item
          = (MessageAdded <$> LH._messagesAdded item)
            <> (MessageDeleted <$> LH._messagesDeleted item)
            <> (extractLabelsAdded <$> LH._labelsAdded item)
            <> (extractLabelsRemoved <$> LH._labelsRemoved item)

newtype PageToken = PageToken Text

paramsFromPageToken :: Maybe PageToken -> [(Text, Text)]
paramsFromPageToken Nothing = []
paramsFromPageToken (Just (PageToken tok)) = [("pageToken", tok)]

gmailUrl a = "https://www.googleapis.com/gmail/v1/users/me" <> a

jsonDecode :: FromJSON a => GmailResponse -> (a -> b) -> Either GmailError b
jsonDecode response f = case response of
  Left (status, message) -> Left $ GmailError status (show message)
  Right json -> (_Left %~ ParsingError) . (_Right %~ f) $ (eitherDecode json)

authCodeUrl :: Text -> Text
authCodeUrl returnPath =
  qndUrl "https://accounts.google.com/o/oauth2/v2/auth" params
  where params =
          [ ("redirect_uri", returnPath)
          , ("prompt", "consent")
          , ("response_type", "code")
          , ("client_id", "676252041546-l6vntlt41kkrddmg1veuk4p761gbgtf0.apps.googleusercontent.com")
          , ("scope", "https://www.googleapis.com/auth/gmail.readonly")
          , ("access_type", "offline")
          ]

requestTokens :: Text -> B.ByteString -> IO GmailTokenInfo
requestTokens redirectUri authCode = do
  B.putStrLn $ "going to request tokens for " <> authCode
  r <- Wreq.asJSON =<< Wreq.post "https://www.googleapis.com/oauth2/v4/token" body
  return (r ^. Wreq.responseBody)
  
  where
    body = [ "code" := authCode
           , "client_id" := gmailClientId
           , "client_secret" := gmailClientSecret
           , "redirect_uri" := redirectUri
           , "grant_type" := ("authorization_code" :: Text)
           ]

type GmailResponse = Either (Int, B.ByteString) B.ByteString

gmailHttp :: Text -> [(Text, Text)] -> StateT GmailTokenInfo IO GmailResponse
gmailHttp path params = do
  token <- get
  liftIO $ putStrLn $ "HTTP GET: " <> path <> ", " <> pack (show params) <> " with tok " <> (decodeUtf8 $ B.toStrict $ authToken token)
  httpResponse <- liftIO $ Wreq.getWith (opts token) (gmailUrl $ unpack path)
  let body = httpResponse ^. Wreq.responseBody
      status = httpResponse ^. Wreq.responseStatus . Wreq.statusCode
  liftIO $ B.putStrLn $ "Response was: " <> (B.pack $ show status) <> ", " <> body
  case status of
    200 -> return $ Right body
    401 -> refreshToken token
    _ -> return $ Left (status, body)
    
  where opts tok = Wreq.defaults & Wreq.params .~ params & theAuth tok & dontThrowExceptions
        theAuth tok = Wreq.auth ?~ Wreq.oauth2Bearer (B.toStrict $ authToken tok)
        authToken = B.pack . unpack . TR.access_token
        refreshCode = B.pack . unpack . TR.refresh_token
        dontThrowExceptions = Wreq.checkResponse .~ Just (\_ _ -> return ())
        refreshToken token = do
          refreshResponse <- liftIO $ refreshTokens (refreshCode token)
          put (updateToken token refreshResponse)
          gmailHttp path params


refreshTokens :: B.ByteString -> IO RefreshResponse
refreshTokens refreshCode = do
  B.putStrLn $ "going to refresh tokens for " <> refreshCode
  r <- Wreq.post "https://www.googleapis.com/oauth2/v4/token" body
  let body = r ^. Wreq.responseBody
  B.putStrLn $ "Got response: " <> body
  return $ fromMaybe RR.emptyRefreshResponse $ decode body
  
  where
    body = [ "refresh_token" := refreshCode
           , "client_id" := gmailClientId
           , "client_secret" := gmailClientSecret
           , "grant_type" := ("refresh_token" :: Text)
           ]

qndUrl :: Text -> [(Text, Text)] -> Text
qndUrl url params = url <> "?" <> intercalate "&" (toEq <$> params)
  where toEq (k,v) = k <> "=" <> v


updateToken :: GmailTokenInfo -> RefreshResponse -> GmailTokenInfo
updateToken original refresh =
  original { TR.access_token = RR.access_token refresh
           , TR.token_type = RR.token_type refresh
           , RR.expires_in = RR.expires_in refresh }
 
