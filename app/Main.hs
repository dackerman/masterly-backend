{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Cli.Display as Cli
-- import qualified Webserver

import           Control.Monad (forever)
import           Data.IORef (newIORef, readIORef, modifyIORef, IORef)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, toLower, stripStart, stripEnd, intercalate)
import qualified Data.Text.IO as TIO
import           System.IO (hSetBuffering, stdout, BufferMode(..))

import           Commands
import           Core
import qualified Integrations.Gmail.JSON.Message as M
import           Integrations.Gmail.Storage (loadMessagesFromStorage)

main :: IO ()
-- main = Webserver.main
-- main = Cli.run
main = hSetBuffering stdout NoBuffering >> simpleRepl

simpleRepl :: IO ()
simpleRepl = do
  appState <- loadState $ user "dackerman"
  app <- newIORef appState
  forever $ do
    putStr "> "
    input <- parse . pack <$> getLine
    process app input

data Command = LoadMail | ListIncoming | Unknown

parse :: Text -> Command
parse input = case (cleaned input) of
  "load mail" -> LoadMail
  "list incoming" -> ListIncoming
  _ -> Unknown

process :: IORef ApplicationState -> Command -> IO ()
process appRef LoadMail = do
  putStrLn "loading mail..."
  messages <- loadMessagesFromStorage
  modifyIORef appRef (\s -> s { mail = toIncomingMessage <$> messages })
  process appRef ListIncoming

process appRef ListIncoming = do
  putStrLn "listing incoming..."
  app <- readIORef appRef
  TIO.putStrLn $ "* " <> (intercalate "\n* " $ renderIncoming <$> (mail app))

process _ _ = do
  putStrLn "Don't know that command"


cleaned :: Text -> Text
cleaned = stripStart . stripEnd . toLower

toIncomingMessage :: M.Message -> Incoming Message
toIncomingMessage = incoming . M . toMail

toMail :: M.Message -> Mail
toMail message = Mail
  { messageID = M._id message
  , from = fromMaybe "?" $ M.extractHeader message "From"
  , to = fromMaybe "?" $ M.extractHeader message "To"
  , subject = M._snippet message
  , body = fromMaybe "" ((M._data . M._body . M._payload) message)
  }

renderIncoming :: Incoming Message -> Text
renderIncoming (Incoming (M mail)) = "[" <> (from mail) <> "] " <> (subject mail)
renderIncoming (Incoming (N (Note note))) = "note: " <> note
