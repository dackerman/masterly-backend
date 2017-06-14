{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.Process
  ( continueFromLastSyncPoint
  , updateExistingMessages
  )
where

import           Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import           Control.Monad (forM, forM_)
import           Control.Monad.State (StateT(..), runStateT, execStateT, evalStateT, gets, modify)
import           Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (foldr)
import           Data.List (any)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Set (difference, union, fromList, toList)
import           Data.Text (Text, unpack)
import           Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
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

continueFromLastSyncPoint :: GmailState -> IO ()
continueFromLastSyncPoint state = case toTGmailState state of
  Nothing -> putStrLn "No API token for syncing!"
  Just oState -> do
    historyChan <- newChan

    (syncCommandsSink, syncCommandsSource) <- spawn $ bounded 1
    (messageResponseSink, messageResponseSource) <- spawn $ unbounded

    let syncCommandsProducer = fetchMessages >-> P.mapM (lift . dispatchSyncCommand) >-> toOutput syncCommandsSink
        bulkDownloader = fromInput syncCommandsSource >-> P.mapM batchGet >-> P.concat >-> toOutput messageResponseSink
        messagePersister = fromInput messageResponseSource >-> P.mapM_ (persistWithChan historyChan)

    -- Preflight gmail request to get new tokens if necessary
    tState <- execStateT preflightGmailTokens oState

    forkIO $ execStateT (runEffect syncCommandsProducer) tState >> performGC

    -- Run 10 downloaders in parallel for better network utilization
    --forM_ [1..10] (\_ -> do
    forkIO $ execStateT (runEffect bulkDownloader) tState >> performGC

    forkIO $ syncLastHistoryMessage historyChan >> performGC

    runEffect messagePersister
    putStrLn "Gmail Processing is complete!"

bailWithoutState :: (TGmailState -> IO ()) -> GmailState -> IO ()
bailWithoutState f state = do
  case toTGmailState state of
    Just tState -> f tState
    Nothing -> putStrLn "No API token for syncing!"

updateExistingMessages = bailWithoutState updateExistingMessagesT

updateExistingMessagesT :: TGmailState -> IO ()
updateExistingMessagesT state = do
  batches <- (chunked 100 . fmap syncMinimal . filter notInInbox) <$> loadMessagesFromStorage
  let queryForUpdates = preflightGmailTokens >> forM batches batchGet
  (results, creds) <- runStateT queryForUpdates state
  forM_ (concat results) persist
  storeIntegrationState $ toGmailState creds

preflightGmailTokens :: Process ()
preflightGmailTokens = toGmailApiCall doPreflightRequest

syncMinimal :: M.Message -> SyncCommand
syncMinimal msg = DoSyncMessage $ SM (MR.MessageRef (M._id msg) (M._threadId msg)) Minimal

notInInbox :: M.Message -> Bool
notInInbox = inBoth . M._labelIds
  where inBoth ls = any ((==) "INBOX") ls &&  any ((==) "UNREAD") ls

data ChunkState a = CS Int [a] [[a]]

chunked :: Int -> [a] -> [[a]]
chunked n l | n < 1 = [l]
chunked n l = fromChunkState $ foldr chunk (CS (n + 1) [] []) l
  where chunk c (CS 1 cs xs) = CS n [c] (xs ++ [cs])
        chunk c (CS v cs xs) = CS (v - 1) (cs ++ [c]) xs

fromChunkState :: ChunkState a -> [[a]]
fromChunkState (CS _ x xs) = xs ++ [x]

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
  currentTimestamp <- liftIO $ formatTime defaultTimeLocale "%F" <$> getCurrentTime
  liftIO $ putStrLn $ currentTimestamp ++ " batch GET of " ++ (show . length) commands ++ " records"
  toGmailApiCall (batchGetMessages (catMaybes $ toRef <$> commands))
  where
    toRef (DoSyncMessage (SM ref format)) = Just $ BatchGetMessage ref format
    toRef _ = Nothing

persist :: BatchGetResponse -> IO ()
persist (FullMessage message) = saveMessageToStorage message
persist (MinimalMessage labels) = do
  maybeExistingMessage <- loadMessageFromStorage labels
  case maybeExistingMessage of
    Just message -> saveMessageToStorage (mergeLabels labels message)
    Nothing -> putStrLn $ "No message to update labels, skipping " ++ show (ML._id labels)

persistWithChan :: Chan Text -> BatchGetResponse -> IO ()
persistWithChan historyChan fm@(FullMessage message) =
  writeChan historyChan (M._historyId message) >> persist fm
persistWithChan historyChan mm@(MinimalMessage messageLabels) =
  writeChan historyChan (ML._historyId messageLabels) >> persist mm


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
