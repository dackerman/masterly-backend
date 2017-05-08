{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Gmail.JSON.MessageLabels where

import Data.Text (Text)
import Data.Int (Int32)
import Data.Aeson.TH

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

