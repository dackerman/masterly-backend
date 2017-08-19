{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Webserver where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, tryReadMVar, putMVar)
import           Control.Monad (void, forM_)
import           Data.Aeson (ToJSON)
import qualified Data.ByteString as B
import           Data.Map (Map, fromList, lookup)
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack, unpack)
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy as LT
import           Data.Vector (Vector)
import qualified Options.Applicative as O
import           Web.Scotty hiding (header)

import           Integration
import           Integrations.Gmail (gmailIntegration)
import           Model.EmailMessage (Grouping, Message, EmailAddress)
import           Service.Gmail (loadState, expectRight, runService, queryMessages)
import           Web.Inbox (inboxEndpoint)

import           Prelude hiding (lookup)

data CLIOptions = Opts
  { command :: Command }

data Command = Sync | Serve

programOpts = O.info
  (optsParser O.<**> O.helper)
  (O.fullDesc)

optsParser :: O.Parser CLIOptions
optsParser = Opts <$> O.subparser
       ( O.command "sync"
         (O.info (pure Sync) (O.progDesc "Sync integrations"))
      <> O.command "serve"
         (O.info (pure Serve) (O.progDesc "Serve web"))
       )

main :: IO ()
main = do
  opts <- O.execParser programOpts
  case command opts of
    Sync -> every (minutes 1) syncIntegrations
    Serve -> app

integrations :: Map Text Integration
integrations = fromList $ (\i -> (integrationId i, i)) <$> ints
  where ints = [gmailIntegration]

servingPort :: Int
servingPort = 8080

baseUrl :: Text
baseUrl = "http://localhost:" <> pack (show servingPort)

cache :: MVar a -> IO a -> IO a
cache var action = do
  putStrLn "doing cached action..."
  cached <- tryReadMVar var
  case cached of
    Just c -> do
      putStrLn "returning cached value!"
      return c
    Nothing -> do
      putStrLn "running action..."
      newValue <- action
      putStrLn "action complete."
      putMVar var newValue
      putStrLn "cached value, returning."
      return newValue

app :: IO ()
app = do
  var <- newEmptyMVar
  scotty servingPort $ do
    post (capture "/api/integrations/:integration") handleIntegration
    get (capture "/api/integrations/:integration") handleIntegration
    get "/" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      json =<< liftAndCatchIO (cache var runInbox)
    post "/login" $ do
      addHeader "Access-Control-Allow-Origin" "*"
      googleTokenId <- param "googleTokenId"
      text $ "cool bean: " <> googleTokenId

runInbox :: IO (Vector (Grouping Message))
runInbox = do
  state <- loadState >>= expectRight
  messagesResult <- runService state queryMessages
  case messagesResult of
    Left errorMsg -> error $ show errorMsg
    Right messages -> inboxEndpoint messages

handleIntegration :: ActionM ()
handleIntegration = do
  integrationName <- param "integration"
  allParams <- fromList <$> params
  case lookup integrationName integrations of
    Just integration -> do
      bytes <- liftAndCatchIO $ loadIntegrationState integrationName
      commands <- liftAndCatchIO $ (setupIntegration integration) bytes allParams
      executeIntegrationCommands integrationName commands
    Nothing -> redirect "/"


executeIntegrationCommands :: Text -> [IntegrationCommand] -> ActionM ()
executeIntegrationCommands _ [] = text "ok"
executeIntegrationCommands integrationName (Redirect pathFn : _) =
  redirect $ fromStrict $ pathFn (baseUrl <> "/api/integrations/" <> integrationName)
executeIntegrationCommands integrationName (StoreState bytes : cs) = do
  liftAndCatchIO $ storeIntegrationState integrationName bytes
  executeIntegrationCommands integrationName cs

executeTaskCommand :: Integration -> TaskCommand -> IO ()
executeTaskCommand _ (UpdateRecord ident doc fields) = do
  putStrLn $ "Updating record " <> show ident
  storeRecord ident doc fields
  return ()
executeTaskCommand integration (UpdateState bytes) = do
  let name = integrationId integration
  putStrLn $ "Saving state for " <> unpack name
  storeIntegrationState name bytes

syncIntegrations :: IO ()
syncIntegrations = void $ traverse syncIntegration integrations

seconds :: Int -> Int
seconds s = s * 1000000

minutes m = 60 * seconds m
hours h = 60 * minutes h

every :: Int -> IO a -> IO ()
every timeUnit action = do
  void $ do
    action
    threadDelay timeUnit
    every timeUnit action


syncIntegration :: Integration -> IO ()
syncIntegration integration = do
  let name = integrationId integration
  putStrLn $ "syncing integration " <> unpack name
  bytes <- loadIntegrationState name
  commands <- (refresh integration) bytes
  forM_ commands (executeTaskCommand integration)
  putStrLn "done syncing Integrations\n\n"


loadIntegrationState :: Text -> IO B.ByteString
loadIntegrationState integrationName = do
  B.readFile (integrationFile integrationName)

storeIntegrationState :: Text -> B.ByteString -> IO ()
storeIntegrationState integrationName bytes =
  B.writeFile (integrationFile integrationName) bytes

storeRecord :: Identifier -> Document -> Fields -> IO ()
storeRecord _ _ _ = return ()
  --BL.writeFile (recordFile ident) serializedRecord
  --where serializedRecord = 0

recordFile :: Identifier -> String
recordFile (Id ident) = "data/db/records/" <> (unpack ident) <> ".json"

integrationFile :: Text -> String
integrationFile name = "data/integrations/" <> (unpack name) <> ".json"
