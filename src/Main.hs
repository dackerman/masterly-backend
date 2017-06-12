{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Cli.Display as Cli
-- import qualified Webserver

import           Control.Monad (forever)
import           Data.IORef (newIORef, readIORef, modifyIORef, IORef)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, toLower, stripStart, stripEnd, intercalate, breakOn)
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

data Command
  = LoadMail
  | ListIncoming
  | ListPrioritized
  | AddNote Text
  | Unknown

parse :: Text -> Command
parse input = case (cleaned input) of
  "load mail" -> LoadMail
  "list incoming" -> ListIncoming
  "list prioritized" -> ListPrioritized
  line -> let (command, arg) = breakOn " " line
          in case command of
               "note" -> AddNote arg
               _ -> Unknown

process :: IORef ApplicationState -> Command -> IO ()
process appRef LoadMail = do
  putStrLn "loading mail..."
  messages <- loadMessagesFromStorage
  modifyIORef appRef (\s -> s { mail = toIncomingMessage <$> messages })
  putStrLn "loaded mail."

process appRef ListIncoming = do
  putStrLn "== INCOMING =="
  app <- readIORef appRef
  TIO.putStrLn $ "* " <> (intercalate "\n* " $ renderIncoming <$> (mail app))

process appRef ListPrioritized = do
  putStrLn "== Prioritized =="
  app <- readIORef appRef
  TIO.putStrLn $ intercalate "\n" $ renderList <$> zip [1..] (tasks $ prioritized app)
  where renderList (i, t) = pack (show i) <> ". " <> renderTaskMessage t

-- TODO: Update the app state with the new note here
process appRef (AddNote note) = do
  modifyIORef appRef (\s -> s { prioritized = prioritizeTask (noteToMessage (Note note)) 0 (prioritized s) })
  putStrLn "Saved note."


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
renderIncoming (Incoming msg) = renderMessage msg

renderTaskMessage :: Task Message -> Text
renderTaskMessage (Task status _ m) = pack (show status) <> ": " <> renderMessage m

renderMessage :: Message -> Text
renderMessage (M mail) = renderMail mail
renderMessage (N note) = renderNote note

renderMail :: Mail -> Text
renderMail mail = "[" <> (from mail) <> "] " <> (subject mail)

renderNote :: Note -> Text
renderNote (Note note) = "note: " <> note
