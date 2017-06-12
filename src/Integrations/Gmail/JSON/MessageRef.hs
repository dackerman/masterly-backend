{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.JSON.MessageRef where

import           Data.Aeson.TH
import           Data.Text (Text)
import qualified Integrations.Gmail.Core as C

data MessageRef = MessageRef
  { _id :: Text
  , _threadId :: Text
  } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''MessageRef)

instance C.HasMessageRef MessageRef where
  getRef = _id
