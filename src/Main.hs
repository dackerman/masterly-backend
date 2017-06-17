{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cli.Display as Cli
import qualified Webserver

import           Control.Monad (forever)
import           Data.IORef (newIORef, readIORef, modifyIORef, IORef)
import           Data.List (foldl', length, sortOn)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import qualified Data.Vector as Vec
import           System.IO (hSetBuffering, stdout, BufferMode(..))

import qualified Brick.BChan as BC

import           Commands
import           Core
import qualified Integrations.Gmail.JSON.Message as M
import           Integrations.Gmail.Storage (loadMessagesFromStorage)

import           Prelude hiding (length, replicate)

main :: IO ()
-- main = hSetBuffering stdout NoBuffering >> Webserver.main
-- main = Cli.main
main = hSetBuffering stdout NoBuffering >> simpleRepl

simpleRepl :: IO ()
simpleRepl = do
  appState <- loadState $ user "dackerman"
  app <- newIORef appState
  chan <- BC.newBChan 1
  Cli.main chan (handler app chan)
--  forever $ do
--    putStr "> "
--    input <- parse . T.pack <$> getLine
--    process app input

type CliChan = BC.BChan Cli.AppEvents

handler :: IORef ApplicationState -> CliChan -> Text -> IO ()
handler app chan text = process app chan (parse text) >> done chan

data Command
  = LoadMail
  | ListIncoming
  | ListPrioritized
  | AddNote Text
  | SyncGmail
  | Unknown Text

parse :: Text -> Command
parse input = case (cleaned input) of
  "load mail" -> LoadMail
  "list incoming" -> ListIncoming
  "list prioritized" -> ListPrioritized
  "sync gmail" -> SyncGmail
  line -> let (command, arg) = T.breakOn " " line
          in case command of
               "note" -> AddNote arg
               t -> Unknown t

notInInbox :: M.Message -> Bool
notInInbox = inBoth . M._labelIds
  where inBoth ls = any ((==) "INBOX") ls &&  any ((==) "UNREAD") ls

lengthOfIncoming :: Incoming Mail -> Int
lengthOfIncoming (Incoming m) = 2 + (T.length $ from m)

updateStatus :: CliChan -> Text -> IO ()
updateStatus ch = BC.writeBChan ch . Cli.UpdateStatus

renderListMode :: CliChan -> Vec.Vector Text -> IO ()
renderListMode ch = BC.writeBChan ch . Cli.RenderList

done :: CliChan -> IO ()
done ch = BC.writeBChan ch Cli.IOComplete

process :: IORef ApplicationState -> CliChan -> Command -> IO ()
process appRef chan LoadMail = do
  updateStatus chan "loading mail..."
  messages <- filter notInInbox <$> loadMessagesFromStorage
  modifyIORef appRef (\s -> s { mail = toIncomingMessage <$> messages })
  updateStatus chan "loaded mail."
  updateStatus chan "saving application..."
  save appRef
  updateStatus chan "saved."

process appRef chan ListIncoming = do
  app <- readIORef appRef
  let fill = 1 + (foldl' max 0 $ lengthOfIncoming <$> mail app)
      grouped = groupBy senderGrouping $ mail app
  renderListMode chan $ renderGroups fill grouped -- (intercalate "\n* " $ renderIncoming fill <$> (mail app))
  updateStatus chan $ (T.pack . show . length) grouped <> " senders in incoming."

process appRef chan ListPrioritized = do
  app <- readIORef appRef
  renderListMode chan $ Vec.fromList $ renderList <$> zip [1..] (tasks $ prioritized app)
  where renderList (i, t) = T.pack (show i) <> ". " <> renderTaskMessage t

process appRef chan (AddNote note) = do
  modifyIORef appRef (\s -> s { prioritized = prioritizeTask (noteToMessage (Note note)) 0 (prioritized s) })
  save appRef
  updateStatus chan "Saved note."

process _ chan SyncGmail = do
  updateStatus chan "Syncing Gmail"
  Webserver.syncIntegrations
  updateStatus chan "Synced Gmail"

process _ chan (Unknown t) = do
  updateStatus chan $ "Don't know the command \"" <> t <> "\""

data Sender = Sender Text [Incoming Mail]

groupSize :: Sender -> Int
groupSize (Sender _ ms) = length ms

type Grouping = Mail -> Text

senderGrouping :: Grouping
senderGrouping = domain

groupBy :: Grouping -> [Incoming Mail] -> Vec.Vector Sender
groupBy g = Vec.fromList . sortGroupByCount . unmap . foldl' f Map.empty
  where f map mail = Map.alter (addMail mail) (g $ incomingToMail mail) map
        unmap m = (\(k,v) -> Sender k v) <$> Map.toList m
        addMail mail Nothing = Just [mail]
        addMail mail (Just l) = Just $ mail : l

sortGroupByCount :: [Sender] -> [Sender]
sortGroupByCount = sortOn (negate . groupSize)

save :: IORef ApplicationState -> IO ()
save ref = readIORef ref >>= saveState (user "dackerman")

cleaned :: Text -> Text
cleaned = T.stripStart . T.stripEnd . T.toLower

toIncomingMessage :: M.Message -> Incoming Mail
toIncomingMessage = incoming . toMail

toMail :: M.Message -> Mail
toMail message = Mail
  { messageID = M._id message
  , from = fromMaybe "?" $ M.extractHeader message "From"
  , to = fromMaybe "?" $ M.extractHeader message "To"
  , receivedDate = textToTimestamp $ M._internalDate message
  , subject = M._snippet message
  , body = fromMaybe "" ((M._data . M._body . M._payload) message)
  }

incomingToMail :: Incoming Mail -> Mail
incomingToMail (Incoming m) = m

domain :: Mail -> Text
domain m = domain
  where (_, domain) = T.breakOn "@" $ from m

textToTimestamp :: Text -> Maybe UTCTime
textToTimestamp = parseTimeM True defaultTimeLocale "%s000" . T.unpack

renderGroups :: Int -> Vec.Vector Sender -> Vec.Vector Text
renderGroups fill senders = renderSender fill <$> senders

renderSender :: Int -> Sender -> Text
renderSender fill (Sender domain mail) = "* " <> rightFill (fill + 1) domain <> renderSenderMail
  where renderSenderMail = "\n " <> T.intercalate "\n " (renderMail fill . incomingToMail <$> mail)

renderIncoming :: Int -> Incoming Mail -> Text
renderIncoming fill (Incoming msg) = renderMail fill msg

rightFill :: Int -> Text -> Text
rightFill fill rendered = rendered <> T.replicate numBlanks " "
  where numBlanks = max 0 (fill - T.length rendered)

renderTaskMessage :: Task Message -> Text
renderTaskMessage (Task status _ m) = T.pack (show status) <> ": " <> renderMessage 0 m

renderMessage :: Int -> Message -> Text
renderMessage fill (M mail) = renderMail fill mail
renderMessage _ (N note) = renderNote note

renderMail :: Int -> Mail -> Text
renderMail fill mail = (rightFill fill $ "[" <> (from mail) <> "]") <> (subject mail)

renderNote :: Note -> Text
renderNote (Note note) = "note: " <> note
