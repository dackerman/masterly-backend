{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Service.Gmail
  ( runService
  , queryMessages
  , loadState
  , expectRight
  , GmailState
  )
  where

import           Control.Monad.State (StateT, liftIO, runStateT, evalStateT, get, put)
import           Data.Aeson ((.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (catMaybes)
import           Data.Text (Text)

import           Integrations.Gmail.Http (GmailError, PageToken)
import qualified Integrations.Gmail.Http as Http
import           Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageRef as MR

data GmailState =
  GS { tokenInfo :: GmailTokenInfo
     }

getAllPages :: Text -> ServiceAction (GmailResult [MR.MessageRef])
getAllPages = getAllPagesRec [] Nothing

tx :: (s' -> s) -> (s -> s') -> StateT s' IO a -> StateT s IO a
tx to from state = do
  big <- get
  (val, newSmallState) <- liftIO $ runStateT state (from big)
  put (to newSmallState)
  return val

tokenToWhole :: StateT GmailTokenInfo IO a -> StateT GmailState IO a
tokenToWhole = tx GS tokenInfo

getAllPagesRec :: [MR.MessageRef] -> Maybe PageToken -> Text -> ServiceAction (GmailResult [MR.MessageRef])
getAllPagesRec previousRefs page query = do
  result <- tokenToWhole $ Http.queryMessages page query
  case result of
    Left err -> return (Left err)
    Right (messages, maybeNextPage) -> case maybeNextPage of
      Nothing -> return $ Right (previousRefs ++ messages)
      _ -> getAllPagesRec (previousRefs ++ messages) maybeNextPage query

loadState :: IO (Either String GmailState)
loadState = do
  bytes <- BSL.readFile "data/integrations/gmail.json"
  let maybeToken = A.eitherDecode bytes >>= A.parseEither (A.withObject "gmail" parseToken)
  return $ GS <$> maybeToken

expectRight :: Show a => Either a b -> IO b
expectRight (Left a) = error (show a)
expectRight (Right b) = return b


parseToken :: A.Object -> A.Parser GmailTokenInfo
parseToken o = o .: "token"

runService :: GmailState -> ServiceAction a -> IO a
runService state action = evalStateT action state

refsToMessages :: GmailResult [MR.MessageRef] -> ServiceAction (GmailResult [M.Message])
refsToMessages result = case result of
  Left err -> return $ Left err
  Right refs -> do
    messages <- tokenToWhole $ Http.batchGetMessages ((\mr -> Http.BatchGetMessage mr Http.Full) <$> refs)
    return $ Right $ catMaybes $ extractMessage <$> messages

extractMessage :: Http.BatchGetResponse -> Maybe M.Message
extractMessage (Http.FullMessage m) = Just m
extractMessage _ = Nothing

queryMessages :: ServiceAction (GmailResult [M.Message])
queryMessages = getAllPages "in:inbox is:unread" >>= refsToMessages

type GmailResult a = Either GmailError a

type ServiceAction a = StateT GmailState IO a
