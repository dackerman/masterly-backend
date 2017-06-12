{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Gmail.JSON.Message where

import           Data.Aeson.TH
import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Integrations.Gmail.Core as C

data Body = Body
  { _attachmentId :: Maybe Text
  , _size :: Int32
  , _data :: Maybe Text
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Body)

data Header = Header
  { _name :: Text
  , _value :: Text
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Header)

data Payload = Payload
  { _partId :: Maybe Text
  , _mimeType :: Text
  , _filename :: Text
  , _headers :: [Header]
  , _body :: Body
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Payload)

extractHeader :: Message -> Text -> Maybe Text
extractHeader message key = find key (_headers $ _payload message)
  where find _ [] = Nothing
        find key (x:xs) = if (key == _name x)
                          then Just (_value x)
                          else find key xs

data Message = Message
  { _id :: Text
  , _threadId :: Text
  , _labelIds :: [Text]
  , _snippet :: Text
  , _historyId :: Text
  , _internalDate :: Text
  , _payload :: Payload
  , _sizeEstimate :: Int32
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Message)

instance C.HasMessageRef Message where
  getRef = _id
