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
  , batchGetMessages
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
import           Control.Monad.State (StateT, liftIO, get, put)
import           Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Either (rights, lefts)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>), mconcat)
import           Data.Text (unpack, intercalate, Text)
import           Debug.Trace (traceShowId)
import           Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import qualified Integrations.Gmail.JSON.GmailTokenInfo as TR
import qualified Integrations.Gmail.JSON.ListHistoryResponse as LH
import qualified Integrations.Gmail.JSON.ListMessagesResponse as LM
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageLabels as ML
import qualified Integrations.Gmail.JSON.MessageRef as MR
import           Integrations.Gmail.JSON.RefreshResponse (RefreshResponse)
import qualified Integrations.Gmail.JSON.RefreshResponse as RR
import           Integrations.Gmail.MultipartBodyParser (parseBatchResponseBody)
import           Integrations.Secrets (gmailClientId, gmailClientSecret)
import           Network.HTTP.Types.URI (renderQuery)
import           Network.Wreq (FormParam(..))
import qualified Network.Wreq as Wreq

import           Prelude hiding (lookup)


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
  liftIO $ putStrLn $ "GET /messages page=" ++ show maybeToken
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
  liftIO $ putStrLn $ "GET /history startHistory=" ++ unpack startHistoryId ++ " page=" ++ show maybeToken
  response <- traceShowId <$> gmailHttp "/history" params
  return $ jsonDecode response processResponse
  where params = paramsFromPageToken maybeToken ++ [("startHistoryId", startHistoryId)]
        processResponse json = ( mconcat $ extractFromHistoryItem <$> (fromMaybe [] $ LH._history json)
                               , PageToken <$> LH._nextPageToken json)
        extractFromHistoryItem item
          = (MessageAdded <$> (LH.refsAdded item))
            <> (MessageDeleted <$> (LH.refsDeleted item))
            <> (uncurry LabelsAdded <$> LH.labelsAdded item)
            <> (uncurry LabelsRemoved <$> LH.labelsRemoved item)

newtype PageToken = PageToken Text deriving Show

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

data BatchGetMessage = BatchGetMessage MR.MessageRef Format
data BatchGetResponse
  = FullMessage M.Message
  | MinimalMessage ML.Message
  deriving Show

batchGetMessages :: [BatchGetMessage] -> GmailApiCall [BatchGetResponse]
batchGetMessages [] = return []
batchGetMessages reqs = do
  response <- batchGet getRequests
  let parsed = parseBatchResponses reqs response
      successful = rights parsed
  liftIO $ putStrLn $ "Failures: " <> show (lefts parsed)
  liftIO $ putStrLn $ "Got " <> show (length successful) <> " responses from batch get"
  return successful
  where
    getRequests = toGetRequest <$> reqs
    toGetRequest (BatchGetMessage ref format) = (("/gmail/v1/users/me/messages/" <> MR._id ref), [("format", Just $ BSC.pack $ unpack $ formatParam format)])

type HttpParams = [(BS.ByteString, Maybe BS.ByteString)]

parseBatchResponses :: [BatchGetMessage] -> Wreq.Response B.ByteString -> [Either String BatchGetResponse]
parseBatchResponses requests response =
  (uncurry parseBatchResponse) <$> zip requests bodyChunks
  where
    contentTypeHeader = response ^. Wreq.responseHeader "Content-Type"
    bodyChunks = parseBatchResponseBody contentTypeHeader (B.toStrict $ response ^. Wreq.responseBody)

parseBatchResponse :: BatchGetMessage -> B.ByteString -> Either String BatchGetResponse
parseBatchResponse (BatchGetMessage _ Minimal) body = MinimalMessage <$> eitherDecode body
parseBatchResponse (BatchGetMessage _ Full) body = FullMessage <$> eitherDecode body

batchGet :: [(Text, HttpParams)] -> GmailApiCall (Wreq.Response B.ByteString)
batchGet requests = do
  --liftIO $ putStrLn path
  --liftIO $ B.putStrLn body
  withFreshToken doPost
  where
    path = "https://www.googleapis.com/batch"
    opts token = (gmailHttpOptions token []) & Wreq.header "Content-Type" .~ ["multipart/mixed; boundary=batch"]
    body = renderBatchGetBody requests
    doPost token = Wreq.postWith (opts token) path body

renderBatchGetBody :: [(Text, HttpParams)] -> B.ByteString
renderBatchGetBody requests = body
  where
    part :: (Text, HttpParams) -> B.ByteString
    part (path, params) =
      "--batch\r\n" <>
      "Content-Type: application/http\r\n\r\n" <>
      "GET " <> (B.pack $ unpack path) <> (B.fromStrict $ renderQuery True params) <> "\r\n" <>
      "Content-Type: application/json\r\n" <>
      "\r\n"
    body = (mconcat $ fmap part requests) <> "--batch--"

type GmailResponse = Either (Int, B.ByteString) B.ByteString

gmailHttp :: Text -> [(Text, Text)] -> GmailApiCall GmailResponse
gmailHttp path params = do
  httpResponse <- withFreshToken doRequest
  let body = httpResponse ^. Wreq.responseBody
      status = httpResponse ^. Wreq.responseStatus . Wreq.statusCode
  case status of
    200 -> return $ Right body
    _ -> return $ Left (status, body)

  where
    doRequest token =
      Wreq.getWith (gmailHttpOptions token params) (gmailUrl $ unpack path)


gmailHttpOptions :: GmailTokenInfo -> [(Text, Text)] -> Wreq.Options
gmailHttpOptions token params = Wreq.defaults & Wreq.params .~ params & auth & dontThrowExceptions
  where auth = Wreq.auth ?~ Wreq.oauth2Bearer (B.toStrict $ B.pack $ unpack $ TR.access_token token)
        dontThrowExceptions = Wreq.checkResponse .~ Just (\_ _ -> return ())


withFreshToken :: (GmailTokenInfo -> IO (Wreq.Response a)) -> GmailApiCall (Wreq.Response a)
withFreshToken request = do
  token <- get
  response <- liftIO $ request token
  case (response ^. Wreq.responseStatus . Wreq.statusCode) of
    401 -> refreshToken
    _ -> return response
  where
    refreshToken = do
      token <- get
      refreshResponse <- liftIO $ refreshTokens (B.pack $ unpack $ TR.refresh_token token)
      put (updateToken token refreshResponse)
      withFreshToken request


refreshTokens :: B.ByteString -> IO RefreshResponse
refreshTokens refreshCode = do
  B.putStrLn $ "going to refresh tokens for " <> refreshCode
  r <- Wreq.post "https://www.googleapis.com/oauth2/v4/token" requestBody
  let responseBody = r ^. Wreq.responseBody
  B.putStrLn $ "Got response: " <> responseBody
  return $ fromMaybe RR.emptyRefreshResponse $ decode responseBody

  where
    requestBody = [ "refresh_token" := refreshCode
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
