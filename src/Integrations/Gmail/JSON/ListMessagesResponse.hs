{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.JSON.ListMessagesResponse where

import Data.Aeson.TH
import Data.Text (Text)

import Integrations.Gmail.JSON.MessageRef

data ListMessagesResponse = ListMessagesResponse
  { _messages :: [MessageRef]
  , _nextPageToken :: Maybe Text
  } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ListMessagesResponse)
