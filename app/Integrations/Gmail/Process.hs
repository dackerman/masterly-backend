module Integrations.Gmail.Process
  ( process
  )
where

import           Control.Monad.State (runStateT)
import           Data.Set (difference, union, fromList, toList)
import           Data.Text
import           Integrations.Gmail.Core
import           Integrations.Gmail.Http
import           Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageRef as MR
import           Integrations.Gmail.Storage
import           Pipes
import qualified Pipes.Prelude as P


process :: GmailState -> IO ()
process state = runEffect stream
  where
    stream = fetchMessagesToSync state Nothing >-> P.concat >-> dispatchCommand

data SyncMessage = SM MR.MessageRef
data UpdateLabels = UL MR.MessageRef [Text] [Text]
data DeleteMessage = DM MR.MessageRef

data SyncCommand
  = DoSyncMessage SyncMessage
  | DoUpdateLabels UpdateLabels
  | DoDeleteMessage DeleteMessage

fetchMessagesToSync :: GmailState -> Maybe PageToken -> Producer [(GmailTokenInfo, SyncCommand)] IO ()
fetchMessagesToSync state page = do
  case token state of
    Nothing -> lift $ putStrLn "No API token for syncing!"
    Just auth -> fetchMessages auth
  where
    fetchMessages auth =
      case lastMessageID state of
        Nothing -> fullSync auth page
        Just history -> historySync auth history page

    fullSync auth page = do
      (response, newToken) <- lift $ runStateT (listMessages page) auth
      case response of
        Left error -> lift $ putStrLn $ show error
        Right (messageRefs, nextPage) -> do
          yield $ (\h -> (newToken, DoSyncMessage $ SM h)) <$> messageRefs
          case nextPage of
            Nothing -> return ()
            _ -> fullSync newToken nextPage

    historySync auth history page = do
      (response, newToken) <- lift $ runStateT (listHistory history page) auth
      case response of
        Left (GmailError 404 _) -> fetchMessagesToSync (state {lastMessageID = Nothing}) page
        Left error -> lift $ putStrLn $ show error
        Right (histories, nextPage) -> do
          yield $ (\h -> (newToken, historyToSyncCommand h)) <$> histories
          case nextPage of
            Nothing -> return ()
            _ -> historySync newToken history nextPage

    historyToSyncCommand (MessageAdded ref) = DoSyncMessage $ SM ref
    historyToSyncCommand (MessageDeleted ref) = DoDeleteMessage $ DM ref
    historyToSyncCommand (LabelsAdded ref labels) = DoUpdateLabels $ UL ref labels []
    historyToSyncCommand (LabelsRemoved ref labels) = DoUpdateLabels $ UL ref [] labels


dispatchCommand :: Consumer (GmailTokenInfo, SyncCommand) IO ()
dispatchCommand = do
  (auth, command) <- await
  lift $ dispatchSyncCommand auth command
  dispatchCommand

dispatchSyncCommand :: GmailTokenInfo -> SyncCommand -> IO ()
dispatchSyncCommand token command = do
  case command of
    DoSyncMessage (SM ref) -> do
      putStrLn $ "Syncing message " ++ show ref
      maybeExistingMessage <- loadMessageFromStorage ref
      case maybeExistingMessage of
        Nothing -> do
          maybeMessage <- downloadMessage token ref
          case maybeMessage of
            Just message -> saveMessageToStorage message
            _ -> return ()
        _ -> do
          putStrLn $ "Message " ++ show ref ++ " already exists, not syncing"
          return ()
        
    DoUpdateLabels command@(UL ref _ _) -> do
      putStrLn $ "Updating labels for " ++ show ref
      maybeMessage <- downloadMessage token ref
      case maybeMessage of
        Just message -> saveMessageToStorage (updateLabelsOnMessage command message)
        _ -> return ()
        
    DoDeleteMessage (DM ref) -> do
      putStrLn $ "Deleting message " ++ show ref
      deleteMessage ref


downloadMessage :: GmailTokenInfo -> MR.MessageRef -> IO (Maybe M.Message)
downloadMessage token ref = do
  (response, _) <- runStateT (loadMessage ref) token
  case response of
    Left error -> putStrLn (show error) >> return Nothing
    Right message -> return $ Just message  


updateLabelsOnMessage :: UpdateLabels -> M.Message -> M.Message
updateLabelsOnMessage (UL _ addedList removedList) original = original { M._labelIds = toList final }
  where added = fromList addedList
        removed = fromList removedList
        current = fromList (M._labelIds original)
        final = difference (union current added) removed
