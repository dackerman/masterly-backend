{-# LANGUAGE DeriveGeneric #-}

module Integrations.Gmail.Core where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics
import Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)

emptyGmailState :: GmailState
emptyGmailState = MkGmail Nothing Nothing

data GmailState = MkGmail
  { token :: Maybe GmailTokenInfo
  , lastMessageID :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON GmailState
instance ToJSON GmailState
