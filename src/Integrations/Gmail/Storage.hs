module Integrations.Gmail.Storage where

import           Control.Monad (mapM)
import           Data.Aeson (eitherDecode, decodeStrict', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>))
import           Data.Text (unpack, Text)
import           Integrations.Gmail.Core
import qualified Integrations.Gmail.JSON.Message as M
import           System.Directory (removeFile, listDirectory, doesFileExist)
import           System.IO.Error (catchIOError, ioError, isDoesNotExistError)

loadMessageFromStorage :: HasMessageRef a => a -> IO (Maybe M.Message)
loadMessageFromStorage ref = catchIOError loadMessage handleError
  where
    loadMessage = do
      bytes <- readMessageFile
      case eitherDecode bytes of
        Left error -> do
          putStrLn $ "Parsing Error: " ++ error
          return Nothing
        Right value -> return $ Just value
    readMessageFile = BL.readFile (messageFilePath $ getRef ref)
    handleError err =
      if isDoesNotExistError err
      then
        return Nothing
      else
        ioError err

saveMessageToStorage :: M.Message -> IO ()
saveMessageToStorage m = do
  BL.writeFile (messageFilePath $ M._id m) (encode m)

deleteMessage :: HasMessageRef a => a -> IO ()
deleteMessage ref = do
  fileExists <- doesFileExist path
  if fileExists
    then removeFile path
    else return ()
  where path = messageFilePath $ getRef ref

messageFilePath :: Text -> String
messageFilePath msgId = "data/integrations/gmail/message_" ++ (unpack msgId)

loadMessagesFromStorage = loadMessagesFromStoragef id

loadMessagesFromStoragef :: ([FilePath] -> [FilePath]) -> IO [M.Message]
loadMessagesFromStoragef f = do
  files <- (fmap appendDir) . f <$> listDirectory "data/integrations/gmail/"
  catMaybes <$> mapM (fmap decodeStrict' . BS.readFile) files
  where appendDir f = "data/integrations/gmail/" <> f

loadNMessagesFromStorage :: Int -> IO [M.Message]
loadNMessagesFromStorage n = loadMessagesFromStoragef (take n)
