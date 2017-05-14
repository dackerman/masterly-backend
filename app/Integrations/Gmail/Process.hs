{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.Process
  ( process
  )
where

import           Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import           Control.Monad.State (StateT(..), runStateT, execStateT, gets, modify)
import           Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Set (difference, union, fromList, toList)
import           Data.Text hiding (head, take)
import           Integrations.Gmail.Core
import           Integrations.Gmail.Http
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageLabels as ML
import qualified Integrations.Gmail.JSON.MessageRef as MR
import           Integrations.Gmail.Storage
import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude as P

import           Prelude hiding (log)

process :: GmailState -> IO ()
process state = case toTGmailState state of
  Nothing -> putStrLn "No API token for syncing!"
  Just tState -> do
    historyChan <- newChan

    (syncCommandsSink, syncCommandsSource) <- spawn $ bounded 5
    (messageResponseSink, messageResponseSource) <- spawn $ unbounded

    let syncCommandsProducer = fetchMessages >-> P.mapM (lift . dispatchSyncCommand) >-> toOutput syncCommandsSink
        bulkDownloader = fromInput syncCommandsSource >-> P.mapM batchGet >-> P.concat >-> toOutput messageResponseSink
        messagePersister = fromInput messageResponseSource >-> P.mapM_ (persist historyChan)

    forkIO $ void $ execStateT (runEffect syncCommandsProducer) tState

    -- Run 3 downloaders in parallel for better network utilization
    forkIO $ void $ execStateT (runEffect bulkDownloader) tState
    forkIO $ void $ execStateT (runEffect bulkDownloader) tState
    forkIO $ void $ execStateT (runEffect bulkDownloader) tState

    forkIO $ syncLastHistoryMessage historyChan

    runEffect messagePersister
    putStrLn "Gmail Processing is complete!"


loadIntegrationState :: IO GmailState
loadIntegrationState = do
  maybeState <- decode <$> BL.readFile integrationFile
  return $ fromMaybe emptyGmailState maybeState

storeIntegrationState :: GmailState -> IO ()
storeIntegrationState state =
  BL.writeFile integrationFile (encode state)

integrationFile :: String
integrationFile = "data/integrations/gmail.json"

syncLastHistoryMessage :: Chan Text -> IO ()
syncLastHistoryMessage historyChan = do
  state <- loadIntegrationState
  processChannel state
  where
    processChannel state = do
      historyValue <- readChan historyChan
      case mergeIfNewer state historyValue of
        Nothing -> do
          processChannel state
        Just newState -> do
          putStrLn $ "updating history to " ++ unpack historyValue
          storeIntegrationState newState
          processChannel newState

mergeIfNewer :: GmailState -> Text -> Maybe GmailState
mergeIfNewer state history = case (lastMessageID state) of
  Nothing -> Just $ state { lastMessageID = Just history }
  Just existingHistory -> if nExisting < nNew then
    Just $ state { lastMessageID = Just history } else
    Nothing
    where nExisting = (read . unpack) existingHistory :: Int
          nNew = (read . unpack) history :: Int


type Process = StateT TGmailState IO

data SyncMessage = SM MR.MessageRef Format
data UpdateLabels = UL MR.MessageRef [Text] [Text]
data DeleteMessage = DM MR.MessageRef

data SyncCommand
  = DoSyncMessage SyncMessage
  | DoUpdateLabels UpdateLabels
  | DoDeleteMessage DeleteMessage


fetchMessages :: Producer [SyncCommand] Process ()
fetchMessages = do
  history <- gets lastMessageID
  case history of
    Nothing -> fullSync Nothing
    Just historyId -> historySync historyId Nothing


toGmailApiCall :: GmailApiCall a -> Process a
toGmailApiCall call = do
  auth <- gets token
  (response, newToken) <- lift $ runStateT call auth
  modify (\s -> s { token = newToken })
  return response


fullSync :: Maybe PageToken -> Producer [SyncCommand] Process ()
fullSync page = do
  response <- lift $ toGmailApiCall (listMessages page)
  case response of
    Left error -> log $ show error
    Right (messageRefs, nextPage) -> do
      yield $ (\ref -> DoSyncMessage $ SM ref Full) <$> messageRefs
      case nextPage of
        Nothing -> return ()
        _ -> fullSync nextPage

historySync :: Text -> Maybe PageToken -> Producer [SyncCommand] Process ()
historySync history page = do
  response <- lift $ toGmailApiCall (listHistory history page)
  case response of
    Left (GmailError 404 _) -> fullSync page
    Left error -> log $ show error
    Right (histories, nextPage) -> do
      yield $ historyToSyncCommand <$> histories
      case nextPage of
        Nothing -> return ()
        _ -> historySync history nextPage

historyToSyncCommand (MessageAdded ref) = DoSyncMessage $ SM ref Full
historyToSyncCommand (MessageDeleted ref) = DoDeleteMessage $ DM ref
historyToSyncCommand (LabelsAdded ref labels) = DoUpdateLabels $ UL ref labels []
historyToSyncCommand (LabelsRemoved ref labels) = DoUpdateLabels $ UL ref [] labels

batchGet :: [SyncCommand] -> Process [BatchGetResponse]
batchGet commands = do
  toGmailApiCall (batchGetMessages (catMaybes $ toRef <$> commands))
  where
    toRef (DoSyncMessage (SM ref format)) = Just $ BatchGetMessage ref format
    toRef _ = Nothing

persist :: Chan Text -> BatchGetResponse -> IO ()
persist historyChan (FullMessage message) = do
  writeChan historyChan (M._historyId message)
  saveMessageToStorage message

persist historyChan (MinimalMessage messageLabels) = do
  writeChan historyChan (ML._historyId messageLabels)
  maybeExistingMessage <- loadMessageFromStorage messageLabels
  case maybeExistingMessage of
    Just message -> saveMessageToStorage (mergeLabels messageLabels message)
    Nothing -> putStrLn $ "No message to update labels, skipping " ++ show (ML._id messageLabels)


dispatchSyncCommand :: [SyncCommand] -> IO [SyncCommand]
dispatchSyncCommand commands = catMaybes <$> mapM doDispatch commands
  where
    doDispatch (DoSyncMessage (SM ref _)) = do
      --log $ "Syncing message " ++ show ref
      maybeExistingMessage <- liftIO $ loadMessageFromStorage ref
      case maybeExistingMessage of
        Nothing -> return $ Just (DoSyncMessage (SM ref Full))
        _ -> return $ Just (DoSyncMessage (SM ref Minimal))

    doDispatch (DoUpdateLabels command@(UL ref _ _)) = do
        log $ "Updating labels for " ++ show ref
        maybeExistingMessage <- liftIO $ loadMessageFromStorage ref
        case maybeExistingMessage of
          Just message -> do
            saveMessageToStorage (updateLabelsOnMessage command message)
            return Nothing
          _ -> do
            log $ "Don't actually have the message, need to full sync " ++ show ref
            return $ Just $ DoSyncMessage (SM ref Full)

    doDispatch (DoDeleteMessage (DM ref)) = do
        log $ "Deleting message " ++ show ref
        deleteMessage ref
        return Nothing


updateLabelsOnMessage :: UpdateLabels -> M.Message -> M.Message
updateLabelsOnMessage (UL _ addedList removedList) original = original { M._labelIds = toList final }
  where added = fromList addedList
        removed = fromList removedList
        current = fromList (M._labelIds original)
        final = difference (union current added) removed

mergeLabels :: ML.Message -> M.Message -> M.Message
mergeLabels messageLabels original = original { M._labelIds = ML._labelIds messageLabels }

log :: MonadIO m => String -> m ()
log = liftIO . putStrLn
