{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Integrations.Gmail.JSON.GmailTokenInfo where

import Data.Aeson
import Data.Text
import GHC.Generics

data GmailTokenInfo = GmailTokenInfo
  { access_token :: Text
  , refresh_token :: Text
  , token_type :: Text
  , expires_in :: Int
  } deriving (Show, Generic)

instance FromJSON GmailTokenInfo
instance ToJSON GmailTokenInfo

emptyGmailTokenInfo :: GmailTokenInfo
emptyGmailTokenInfo = GmailTokenInfo
  { access_token = ""
  , refresh_token = ""
  , token_type = ""
  , expires_in = -1
  }
