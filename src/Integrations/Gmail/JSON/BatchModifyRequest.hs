{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Integrations.Gmail.JSON.BatchModifyRequest where

import Data.Aeson.TH
import Data.Text (Text)

data BatchModifyRequest = BatchModifyRequest
  { _ids :: [Text]
  , _addLabelIds :: [Text]
  , _removeLabelIds :: [Text]
  } deriving (Show)

emptyRequest :: BatchModifyRequest
emptyRequest = BatchModifyRequest mempty mempty mempty

addId :: Text -> BatchModifyRequest -> BatchModifyRequest
addId id r = r { _ids = id : _ids r }

addLabel :: Text -> BatchModifyRequest -> BatchModifyRequest
addLabel label r = r { _addLabelIds = label : _addLabelIds r }

removeLabel :: Text -> BatchModifyRequest -> BatchModifyRequest
removeLabel label r = r { _removeLabelIds = label : _removeLabelIds r }

$(deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''BatchModifyRequest)
