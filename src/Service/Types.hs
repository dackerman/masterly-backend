{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Service.Types where

import Data.Text (Text)

import Integrations.Gmail.JSON.GmailTokenInfo (GmailTokenInfo)
import Model.EmailMessage (Message)

data Account = Account
  { email :: Text
  , tokenInfo :: GmailTokenInfo
  }

newtype AccountId = AccountId Text

class Monad m => Accounts m where

  register :: Account -> m ()

  lookup :: AccountId -> m (Maybe Account)


class Monad m => GmailService m where

  loadMail :: GmailTokenInfo -> m [Message]
