{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Gmail.JSON.ModifyRequest where

import Data.Aeson.TH
import Data.Text (Text)

data ModifyRequest = ModifyRequest
  { _addLabelIds :: [Text]
  , _removeLabelIds :: [Text]
  } deriving (Show)

emptyRequest :: ModifyRequest
emptyRequest = ModifyRequest mempty mempty

addLabel :: Text -> ModifyRequest -> ModifyRequest
addLabel label r = r { _addLabelIds = label : _addLabelIds r }

removeLabel :: Text -> ModifyRequest -> ModifyRequest
removeLabel label r = r { _removeLabelIds = label : _removeLabelIds r }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''ModifyRequest)
