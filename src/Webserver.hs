{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Webserver where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (void, forM_)
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map, fromList, lookup)
import           Data.Semigroup ((<>))
import           Data.Text (Text, pack, unpack)
import           Data.Text.Lazy (fromStrict)
import qualified Options.Applicative as O
import           Web.Scotty hiding (header)

import           Integration
import           Integrations.Gmail (gmailIntegration)

import           Prelude hiding (lookup)

data CLIOptions = Opts
  { staticDir :: String }

optsParser :: O.Parser CLIOptions
optsParser = Opts
  <$> O.strOption
  ( O.long "static" <> O.help "Directory to serve static files")

programOpts = O.info
  (optsParser O.<**> O.helper)
  (O.fullDesc)

main :: IO ()
main = do
  -- opts <- execParser programOpts
  -- syncIntegrations
  app

integrations :: Map Text Integration
integrations = fromList $ (\i -> (integrationId i, i)) <$> ints
  where ints = [gmailIntegration]

servingPort :: Int
servingPort = 8080

baseUrl :: Text
baseUrl = "http://localhost:" <> pack (show servingPort)

app :: IO ()
app = scotty servingPort $ do
  post (capture "/api/integrations/:integration") handleIntegration
  get (capture "/api/integrations/:integration") handleIntegration
  get "/" $ do
    text "hey there!"

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
  void $ forkIO $ do
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
  putStrLn "done syncing Integrations"


loadIntegrationState :: Text -> IO BL.ByteString
loadIntegrationState integrationName = do
  BL.readFile (integrationFile integrationName)

storeIntegrationState :: Text -> BL.ByteString -> IO ()
storeIntegrationState integrationName bytes =
  BL.writeFile (integrationFile integrationName) bytes

storeRecord :: Identifier -> Document -> Fields -> IO ()
storeRecord _ _ _ = return ()
  --BL.writeFile (recordFile ident) serializedRecord
  --where serializedRecord = 0

recordFile :: Identifier -> String
recordFile (Id ident) = "data/db/records/" <> (unpack ident) <> ".json"

integrationFile :: Text -> String
integrationFile name = "data/integrations/" <> (unpack name) <> ".json"
