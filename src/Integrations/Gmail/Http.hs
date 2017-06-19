{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

module Integrations.Gmail.Http
  ( listMessages
  , listHistory
  , loadMessage
  , loadMessageLabels
  , archiveMessage
  , archiveMessages
  , requestTokens
  , authCodeUrl
  , batchGetMessages
  , doPreflightRequest
  , GmailApiCall
  , GmailError(..)
  , History(..)
  , PageToken(..)
  , BatchGetMessage(..)
  , BatchGetResponse(..)
  , Format(..)
  )
where

import           Control.Lens ((&), (.~), (^.), (?~), (%~))
import           Control.Lens.Prism (_Left, _Right)
import           Control.Monad (void)
import           Control.Monad.State (StateT, liftIO, get, put)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), mconcat)
import           Data.String (IsString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Text (Text)
import qualified Data.Text as T

import           Integrations.Gmail.JSON.BatchModifyRequest (BatchModifyRequest)
import qualified Integrations.Gmail.JSON.BatchModifyRequest as BM
import           Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import qualified Integrations.Gmail.JSON.GmailTokenInfo as TR
import qualified Integrations.Gmail.JSON.ListHistoryResponse as LH
import qualified Integrations.Gmail.JSON.ListMessagesResponse as LM
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageLabels as ML
import qualified Integrations.Gmail.JSON.MessageRef as MR
import qualified Integrations.Gmail.JSON.ModifyRequest as MoR
import           Integrations.Gmail.JSON.RefreshResponse (RefreshResponse)
import qualified Integrations.Gmail.JSON.RefreshResponse as RR
import           Integrations.Gmail.MultipartBodyParser (parseBatchResponseBody)
import           Integrations.Secrets (gmailClientId, gmailClientSecret)

import           Data.Aeson
import           Network.HTTP.Types.URI (renderQuery)
import           Network.Wreq (FormParam(..))
import qualified Network.Wreq as Wreq

import           Prelude hiding (lookup)


data GmailError = GmailError Int String | ParsingError String
  deriving Show

type GmailApiCall a = StateT GmailTokenInfo IO a


data History
  = MessageAdded MR.MessageRef
  | MessageDeleted MR.MessageRef
  | LabelsAdded MR.MessageRef [Text]
  | LabelsRemoved MR.MessageRef [Text]

type ListHistoryResponse = ([History], Maybe PageToken)

data Format = Full | Minimal

type ListMessagesResponse = ([MR.MessageRef], Maybe PageToken)

newtype PageToken = PageToken Text deriving Show

data BatchGetMessage = BatchGetMessage MR.MessageRef Format
data BatchGetResponse
  = FullMessage M.Message
  | MinimalMessage ML.Message
  | BatchError B.ByteString
  deriving Show

type HttpParams = [(BS.ByteString, Maybe BS.ByteString)]

type BatchGetError = (B.ByteString, B.ByteString)

loadMessage :: MR.MessageRef -> GmailApiCall (Either GmailError M.Message)
loadMessage = loadMessageGeneric Full

loadMessageLabels :: MR.MessageRef -> GmailApiCall (Either GmailError ML.Message)
loadMessageLabels = loadMessageGeneric Minimal

loadMessageGeneric :: FromJSON a => Format -> MR.MessageRef -> GmailApiCall (Either GmailError a)
loadMessageGeneric format ref = jsonDecode id <$> gmailHttpGet path params
  where path = "/messages/" <> MR._id ref
        params = [("format", formatParam format)]

formatParam :: IsString a => Format -> a
formatParam Full = "full"
formatParam Minimal = "minimal"

listMessages :: Maybe PageToken -> GmailApiCall (Either GmailError ListMessagesResponse)
listMessages maybeToken = do
  response <- gmailHttpGet "/messages" params
  return $ jsonDecode (\m -> (LM._messages m, PageToken <$> LM._nextPageToken m)) response
  where params = paramsFromPageToken maybeToken ++ [("q", "in:inbox is:unread")]

archiveMessage :: Text -> GmailApiCall (Either GmailError ())
archiveMessage ref = do
  let path = "/messages/" <> ref <> "/modify"
  response <- gmailHttpPost path [] body
  return $ const () <$> responseToGmail response
  where body = MoR.removeLabel "INBOX" MoR.emptyRequest

archiveMessages :: [Text] -> GmailApiCall (Either GmailError ())
archiveMessages ids = batchModifyMessages $ BM.BatchModifyRequest ids [] ["INBOX"]

batchModifyMessages :: BatchModifyRequest -> GmailApiCall (Either GmailError ())
batchModifyMessages request = do
  response <- responseToGmail <$> gmailHttpPost "/messages/batchModify" [] request
  return $ const () <$> response

listHistory :: Text -> Maybe PageToken -> GmailApiCall (Either GmailError ListHistoryResponse)
listHistory startHistoryId maybeToken = do
  response <- gmailHttpGet "/history" params
  return $ jsonDecode processResponse response
  where params = paramsFromPageToken maybeToken ++ [("startHistoryId", startHistoryId)]
        processResponse json = ( mconcat $ extractFromHistoryItem <$> (fromMaybe [] $ LH._history json)
                               , PageToken <$> LH._nextPageToken json)
        extractFromHistoryItem item
          = (MessageAdded <$> (LH.refsAdded item))
            <> (MessageDeleted <$> (LH.refsDeleted item))
            <> (uncurry LabelsAdded <$> LH.labelsAdded item)
            <> (uncurry LabelsRemoved <$> LH.labelsRemoved item)

paramsFromPageToken :: Maybe PageToken -> [(Text, Text)]
paramsFromPageToken Nothing = []
paramsFromPageToken (Just (PageToken tok)) = [("pageToken", tok)]

gmailUrl a = "https://www.googleapis.com/gmail/v1/users/me" <> a

jsonDecode :: FromJSON a => (a -> b) -> (Either (Int, B.ByteString) B.ByteString) -> Either GmailError b
jsonDecode f response = case response of
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
          , ("scope", "https://www.googleapis.com/auth/gmail.readonly+https://www.googleapis.com/auth/gmail.modify")
          , ("access_type", "offline")
          ]

requestTokens :: Text -> B.ByteString -> IO GmailTokenInfo
requestTokens redirectUri authCode = do
  r <- Wreq.asJSON =<< Wreq.post "https://www.googleapis.com/oauth2/v4/token" body
  return (r ^. Wreq.responseBody)

  where
    body = [ "code" := authCode
           , "client_id" := gmailClientId
           , "client_secret" := gmailClientSecret
           , "redirect_uri" := redirectUri
           , "grant_type" := ("authorization_code" :: Text)
           ]

batchGetMessages :: [BatchGetMessage] -> GmailApiCall [BatchGetResponse]
batchGetMessages [] = return []
batchGetMessages requests = do
  responses <- batchGet $ convertBatchGetMessage <$> requests
  return $ (uncurry parseBatchResponse) <$> zip requests responses

convertBatchGetMessage :: BatchGetMessage -> (Text, HttpParams)
convertBatchGetMessage (BatchGetMessage ref format) = (path, params)
  where path = "/gmail/v1/users/me/messages/" <> MR._id ref
        params = [("format", Just $ BSC.pack $ T.unpack $ formatParam format)]

parseBatchResponse :: BatchGetMessage -> B.ByteString -> BatchGetResponse
parseBatchResponse (BatchGetMessage _ Minimal) body = toBatchResponse $ MinimalMessage <$> eitherDecodeWithInput body
parseBatchResponse (BatchGetMessage _ Full) body = toBatchResponse $ FullMessage <$> eitherDecodeWithInput body

toBatchResponse :: Either BatchGetError BatchGetResponse -> BatchGetResponse
toBatchResponse (Left (msg, body)) = BatchError $ msg <> body
toBatchResponse (Right r) = r

eitherDecodeWithInput :: FromJSON a => B.ByteString -> Either (B.ByteString, B.ByteString) a
eitherDecodeWithInput input = case eitherDecode input of
  Left error -> Left (B.pack error, input)
  Right val -> Right val

batchGet :: [(Text, HttpParams)] -> GmailApiCall [B.ByteString]
batchGet requests = do
  response <- withFreshToken doPost
  let contentTypeHeader = response ^. Wreq.responseHeader "Content-Type"
      body = response ^. Wreq.responseBody
      bodyChunks = parseBatchResponseBody contentTypeHeader (B.toStrict body)
  return bodyChunks
  where
    path = "https://www.googleapis.com/batch"
    opts token = (gmailHttpOptions token []) & Wreq.header "Content-Type" .~ ["multipart/mixed; boundary=batch"]
    doPost token = Wreq.postWith (opts token) path (renderBatchGetBody requests)
    renderBatchGetBody requests = (mconcat $ fmap part requests) <> "--batch--"
    part (path, params) =
      "--batch\r\n" <>
      "Content-Type: application/http\r\n\r\n" <>
      "GET " <> (B.pack $ T.unpack path) <> (B.fromStrict $ renderQuery True params) <> "\r\n" <>
      "Content-Type: application/json\r\n" <>
      "\r\n"


doPreflightRequest :: GmailApiCall ()
doPreflightRequest = void $ gmailHttpGet "/profile" []

gmailHttpGet :: Text -> [(Text, Text)] -> GmailApiCall (Either (Int, B.ByteString) B.ByteString)
gmailHttpGet path params = gmailHttp doRequest
  where doRequest token = Wreq.getWith (gmailHttpOptions token params) (gmailUrl $ T.unpack path)

gmailHttpPost :: ToJSON a => Text -> [(Text, Text)] -> a -> GmailApiCall (Either (Int, B.ByteString) B.ByteString)
gmailHttpPost path params json = gmailHttp doRequest
  where doRequest token = Wreq.postWith (gmailHttpOptions token params) (gmailUrl $ T.unpack path) body
        body = toJSON json


type GmailRequester a = GmailTokenInfo -> IO (Wreq.Response a)

gmailHttp :: GmailRequester a -> GmailApiCall (Either (Int, a) a)
gmailHttp requester = do
  httpResponse <- withFreshToken requester
  let body = httpResponse ^. Wreq.responseBody
      status = httpResponse ^. Wreq.responseStatus . Wreq.statusCode
  return $ if status >= 200 && status < 300
    then Right body
    else Left (status, body)


gmailHttpOptions :: GmailTokenInfo -> [(Text, Text)] -> Wreq.Options
gmailHttpOptions token params = Wreq.defaults & Wreq.params .~ params & auth & dontThrowExceptions
  where auth = Wreq.auth ?~ Wreq.oauth2Bearer (B.toStrict $ B.pack $ T.unpack $ TR.access_token token)
        dontThrowExceptions = Wreq.checkResponse .~ Just (\_ _ -> return ())


withFreshToken :: GmailRequester a -> GmailApiCall (Wreq.Response a)
withFreshToken request = do
  token <- get
  response <- liftIO $ request token
  case (response ^. Wreq.responseStatus . Wreq.statusCode) of
    401 -> refreshToken
    _ -> return response
  where
    refreshToken = do
      token <- get
      refreshResponse <- liftIO $ refreshTokens (B.pack $ T.unpack $ TR.refresh_token token)
      put (updateToken token refreshResponse)
      withFreshToken request


responseToGmail :: (Either (Int, B.ByteString) B.ByteString) -> Either GmailError B.ByteString
responseToGmail (Right a) = Right a
responseToGmail (Left (status, err)) = Left $ GmailError status (show err)

refreshTokens :: B.ByteString -> IO RefreshResponse
refreshTokens refreshCode = do
  r <- Wreq.post "https://www.googleapis.com/oauth2/v4/token" requestBody
  let responseBody = r ^. Wreq.responseBody
  return $ fromMaybe RR.emptyRefreshResponse $ decode responseBody

  where
    requestBody = [ "refresh_token" := refreshCode
                  , "client_id" := gmailClientId
                  , "client_secret" := gmailClientSecret
                  , "grant_type" := ("refresh_token" :: Text)
                  ]

qndUrl :: Text -> [(Text, Text)] -> Text
qndUrl url params = url <> "?" <> T.intercalate "&" (toEq <$> params)
  where toEq (k,v) = k <> "=" <> v


updateToken :: GmailTokenInfo -> RefreshResponse -> GmailTokenInfo
updateToken original refresh =
  original { TR.access_token = RR.access_token refresh
           , TR.token_type = RR.token_type refresh
           , RR.expires_in = RR.expires_in refresh }
