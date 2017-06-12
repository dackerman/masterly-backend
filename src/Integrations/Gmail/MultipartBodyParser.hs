{-# LANGUAGE OverloadedStrings #-}

module Integrations.Gmail.MultipartBodyParser
  ( parseBatchResponseBody
  )
where

import           Data.ByteString hiding (filter)
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid ((<>))

import           Prelude hiding (null, drop, length)

parseBatchResponseBody :: ByteString -> ByteString -> [BL.ByteString]
parseBatchResponseBody cth body = BL.fromStrict <$> filter (not . null) (parseBatchResponseBody' boundary body)
  where boundary = "--" <> (drop 9 $ snd $ breakSubstring "boundary=" cth)

parseBatchResponseBody' :: ByteString -> ByteString -> [ByteString]
parseBatchResponseBody' contentTypeHeader body =
  let (start, end) = breakSubstring boundary body
  in (extractJson start) : if null end then [] else parseBatchResponseBody' contentTypeHeader (drop (length contentTypeHeader) end)
  where
    boundary = contentTypeHeader
    extractJson bs = let (_, after) = breakSubstring "{" bs
                     in after
