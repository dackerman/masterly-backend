{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

module Integrations.Gmail
  ( gmailIntegration
  , stateFromBytes)
where

import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.Map (lookup)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Integration
import           Integrations.Gmail.Core
import           Integrations.Gmail.Http
import           Integrations.Gmail.Process
import           Prelude hiding (putStrLn, lookup)

gmailIntegration :: Integration
gmailIntegration = MkIntegration
  { integrationId = "gmail"
  , setupIntegration = gmailSetup
  , refresh = gmailRefresh
  }

gmailSetup :: B.ByteString -> IntegrationParams -> IO [IntegrationCommand]
gmailSetup bytes params = do
  let state = stateFromBytes bytes
  case lookup "code" params of
    Nothing -> return [Redirect authCodeUrl]
    Just code -> do
      response <- requestTokens "http://localhost:8080/api/integrations/gmail" (BLC.pack . show $ code)
      return [ StoreState (BL.toStrict . encode $ state {token = Just response})
             , Redirect (const "/")]

gmailRefresh :: B.ByteString -> IO [TaskCommand]
gmailRefresh bytes = do
  let state = stateFromBytes bytes
  BLC.putStrLn $ "Starting gmail sync with " <> BLC.fromStrict bytes
  --updateExistingMessages state
  continueFromLastSyncPoint state
  return []

stateFromBytes :: B.ByteString -> GmailState
stateFromBytes = fromMaybe emptyGmailState . decodeStrict
