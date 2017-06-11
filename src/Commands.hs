{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Commands
  ( loadState
  , saveState
  , user
  , ApplicationState(..)
  , Mail(..)
  , Note(..)
  , Message(..)
  ) where

import           Core
import           Data.Aeson (decode, encode)
import           Data.Aeson.TH (deriveJSON, defaultOptions)
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (Maybe(..))
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text, unpack)
import           System.Directory (doesFileExist)

import           Prelude hiding (readFile)

data Mail = Mail
  { messageID :: Text
  , from :: Text
  , to :: Text
  , subject :: Text
  , body :: Text
  }

newtype Note = Note Text

data Message = M Mail | N Note

data ApplicationState = AppState
  { mail :: [Incoming Message]
  , prioritized :: Prioritized Message
  }

$(deriveJSON defaultOptions ''Mail)
$(deriveJSON defaultOptions ''Note)
$(deriveJSON defaultOptions ''Message)
$(deriveJSON defaultOptions ''Incoming)
$(deriveJSON defaultOptions ''PrioritizedF)
$(deriveJSON defaultOptions ''Task)
$(deriveJSON defaultOptions ''Status)
$(deriveJSON defaultOptions ''TimeLog)
$(deriveJSON defaultOptions ''TimeTrackEvent)
$(deriveJSON defaultOptions ''ApplicationState)

emptyApp :: ApplicationState
emptyApp = AppState
  { mail = mempty
  , prioritized = mempty}

newtype UserName = UserName Text

user :: Text -> UserName
user = UserName

appFileName :: UserName -> FilePath
appFileName (UserName u) = unpack $ u <> "-store.json"

loadState :: UserName -> IO ApplicationState
loadState u = do
  exists <- doesFileExist path
  maybeApp <- case exists of
                True -> decode <$> BSL.readFile path
                False -> pure Nothing
  return $ fromMaybe emptyApp maybeApp
  where path = appFileName u


saveState :: UserName -> ApplicationState -> IO ()
saveState u app = BSL.writeFile (appFileName u) (encode app)
