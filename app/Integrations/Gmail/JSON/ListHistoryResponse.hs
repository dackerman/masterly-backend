{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.JSON.ListHistoryResponse where

import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Integrations.Gmail.JSON.MessageRef

data MessageWithLabels = MessageWithLabels
 { _message :: MessageRef
 , _labelIds :: [Text]
 } deriving (Show)

data Message = Message
  { __message :: MessageRef
  } deriving (Show)

data HistoryItem = HistoryItem
  { _id :: Text
  , _messages :: [MessageRef]
  , _messagesAdded :: Maybe [Message]
  , _messagesDeleted :: Maybe [Message]
  , _labelsAdded :: Maybe [MessageWithLabels]
  , _labelsRemoved :: Maybe [MessageWithLabels]
  } deriving (Show)

data ListMessagesResponse = ListMessagesResponse
  { _history :: Maybe [HistoryItem]
  , _nextPageToken :: Maybe Text
  , _historyId :: Text
  } deriving (Show)

extractRefs :: Maybe [Message] -> [MessageRef]
extractRefs maybeMessages = __message <$> fromMaybe [] maybeMessages

extractLabelRefs :: Maybe [MessageWithLabels] -> [(MessageRef, [Text])]
extractLabelRefs maybeMessages = (\m -> (_message m, _labelIds m)) <$> fromMaybe [] maybeMessages

refsAdded = extractRefs . _messagesAdded
refsDeleted = extractRefs . _messagesDeleted
labelsAdded = extractLabelRefs . _labelsAdded
labelsRemoved = extractLabelRefs . _labelsRemoved

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''MessageWithLabels)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 2} ''Message)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''HistoryItem)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ListMessagesResponse)
