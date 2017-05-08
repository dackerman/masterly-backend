{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.JSON.MessageRef where

import Data.Text (Text)
import Data.Aeson.TH

data MessageRef = MessageRef
  { _id :: Text
  , _threadId :: Text
  } deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''MessageRef)
