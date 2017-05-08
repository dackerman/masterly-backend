module Integrations.Gmail.Storage where

import           Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import           Data.Text (unpack, Text)
import qualified Integrations.Gmail.JSON.Message as M
import qualified Integrations.Gmail.JSON.MessageRef as MR
import           System.Directory (removeFile)
import           System.IO.Error (catchIOError, ioError, isDoesNotExistError)

loadMessageFromStorage :: MR.MessageRef -> IO (Maybe M.Message)
loadMessageFromStorage ref = catchIOError loadMessage handleError
  where
    loadMessage = do
      bytes <- readMessageFile
      case eitherDecode bytes of
        Left error -> do
          putStrLn $ "Parsing Error: " ++ error
          return Nothing
        Right value -> return $ Just value
    readMessageFile = BL.readFile (messageFilePath $ MR._id ref)
    handleError err =
      if isDoesNotExistError err
      then
        return Nothing
      else
        ioError err


saveMessageToStorage :: M.Message -> IO ()
saveMessageToStorage m = do
  BL.writeFile (messageFilePath $ M._id m) (encode m)

deleteMessage :: MR.MessageRef -> IO ()
deleteMessage ref = removeFile (messageFilePath (MR._id ref))

messageFilePath :: Text -> String
messageFilePath msgId = "data/integrations/gmail/message_" ++ (unpack msgId)
