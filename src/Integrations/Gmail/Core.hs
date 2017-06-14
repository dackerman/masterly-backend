{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Integrations.Gmail.Core where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics
import Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)

emptyGmailState :: GmailState
emptyGmailState = MkGmail Nothing Nothing

toTGmailState :: GmailState -> Maybe TGmailState
toTGmailState orig = case token orig of
  Just tok -> Just $ MkGmail tok (lastMessageID orig)
  Nothing -> Nothing

toGmailState :: TGmailState -> GmailState
toGmailState MkGmail{..} = MkGmail (Just token) lastMessageID

data GmailStateG a = MkGmail
  { token :: a
  , lastMessageID :: Maybe Text
  }
  deriving (Show, Generic)

type GmailState = GmailStateG (Maybe GmailTokenInfo)
type TGmailState = GmailStateG GmailTokenInfo

instance FromJSON a => FromJSON (GmailStateG a)
instance ToJSON a => ToJSON (GmailStateG a)

class HasMessageRef a where
  getRef :: a -> Text
