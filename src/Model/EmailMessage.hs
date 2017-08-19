{-# LANGUAGE TemplateHaskell #-}

module Model.EmailMessage where

import Data.Aeson.TH
import qualified Data.Map as Map
import           Data.Text.Lazy (Text)
import           Data.Vector (Vector, (++))
import qualified Data.Vector as Vec

import           Prelude hiding ((++))

data Grouping a = Grouping Text (Vector a)

data EmailAddress = EmailAddress
  { displayName :: Text
  , name :: Text
  , domain :: Text
  } deriving Show

data Header = Header
  { key :: Text
  , value :: Text
  } deriving Show

data Message = Message
  { messageId :: Text
  , subject :: Text
  , snippet :: Text
  , from :: EmailAddress
  , body :: Text
  , dateSent :: Integer
  , headers :: [Header]
  } deriving Show

$(deriveJSON defaultOptions ''EmailAddress)
$(deriveJSON defaultOptions ''Header)
$(deriveJSON defaultOptions ''Message)
$(deriveJSON defaultOptions ''Grouping)

mapGrouping :: (Message -> Text) -> Vector Message -> Vector (Grouping Message)
mapGrouping t messages = fmap group $ Vec.fromList $ Map.toList $ foldr combine Map.empty messages
  where combine msg = Map.insertWith (++) (t msg) (Vec.singleton msg)
        group (name, messages) = Grouping name messages
