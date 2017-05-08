{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.JSON.ListHistoryResponse where

import Data.Text (Text)
import Data.Aeson.TH

import Integrations.Gmail.JSON.MessageRef

data MessageWithLabels = MessageWithLabels
 { _message :: MessageRef
 , _labelIds :: [Text]
 } deriving (Show)

data HistoryItem = HistoryItem
  { _id :: Integer
  , _messages :: [MessageRef]
  , _messagesAdded :: [MessageRef]
  , _messagesDeleted :: [MessageRef]
  , _labelsAdded :: [MessageWithLabels]
  , _labelsRemoved :: [MessageWithLabels]
  } deriving (Show)

data ListMessagesResponse = ListMessagesResponse
  { _history :: [HistoryItem]
  , _nextPageToken :: Maybe Text
  , _historyId :: Text
  } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''MessageWithLabels)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''HistoryItem)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ListMessagesResponse)

