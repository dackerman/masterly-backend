{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Gmail.JSON.MessageLabels where

import           Data.Aeson.TH
import           Data.Int (Int32)
import           Data.Text (Text)
import qualified Integrations.Gmail.Core as C

data Message = Message
  { _id :: Text
  , _threadId :: Text
  , _labelIds :: [Text]
  , _snippet :: Text
  , _historyId :: Text
  , _internalDate :: Text
  , _sizeEstimate :: Int32
  } deriving (Show)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Message)

instance C.HasMessageRef Message where
  getRef = _id
