module Integration where

import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL

newtype Identifier = Id Text
  deriving Show

data FieldType = StringType | NumberType

data FieldDef = FieldDef
  { fieldName :: Text
  , fieldType :: FieldType
  }

type Fields = [FieldDef]

data Document = Doc
  { documentTitle :: Text
  , documentBody :: Text
  }

data TaskCommand =
  UpdateRecord Identifier Document Fields
  | UpdateState ByteString

data IntegrationCommand
  = Redirect (Text -> Text)
  | StoreState ByteString

type IntegrationParams = Map TL.Text TL.Text

data Integration = MkIntegration
  { integrationId :: Text
  , setupIntegration :: ByteString -> IntegrationParams -> IO [IntegrationCommand]
  , refresh :: ByteString -> IO [TaskCommand]
  }
