{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Integrations.Gmail.JSON.RefreshResponse where

import Data.Aeson
import Data.Text
import GHC.Generics

data RefreshResponse = RefreshResponse
  { access_token :: Text
  , token_type :: Text
  , expires_in :: Int
  } deriving (Show, Generic)

instance FromJSON RefreshResponse
instance ToJSON RefreshResponse

emptyRefreshResponse :: RefreshResponse
emptyRefreshResponse = RefreshResponse
  { access_token = ""
  , token_type = ""
  , expires_in = -1
  }
