module DocumentStore
  ( storeDocument
  )
where

import Data.Aeson
import Data.ByteString (ByteString, writeFile)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)

import Integration


import Prelude hiding (writeFile)

storeDocument :: Identifier -> Document -> Fields -> IO ()
storeDocument (Id ident) doc fields = do
  writeFile (filePathForID ident) (serializeDoc doc fields)



serializeDoc :: Document -> Fields -> ByteString
serializeDoc doc fields = _

filePathForID :: Text -> String
filePathForID ident = "data/db/" <> unpack ident
