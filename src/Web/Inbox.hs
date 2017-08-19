{-# LANGUAGE OverloadedStrings #-}

module Web.Inbox
  ( inboxEndpoint
  )
where

import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text, fromStrict, pack, strip)
import           Data.Text.Read (decimal)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Algorithms.Intro as Algo
import           Text.Parsec (runParser, many, noneOf, anyChar, between, char, try, (<|>), manyTill, many1, ParseError)

import qualified Integrations.Gmail.JSON.Message as M
import           Model.EmailMessage (Grouping(..), Header(..), Message(..), EmailAddress(..), mapGrouping)

groupBySender :: Vector Message -> Vector (Grouping Message)
groupBySender = mapGrouping senderGrouping

senderGrouping :: Message -> Text
senderGrouping = domain . from

parseEmailAddress :: Text -> Either ParseError EmailAddress
parseEmailAddress t = runParser p () "email" t
  where p = try emailWithDisplay <|> bareEmail
        emailWithDisplay = do
          displayName <- pack <$> many (try (noneOf "<>"))
          (name, domain) <- between (char '<') (char '>') bareEmail'
          return $ EmailAddress (strip displayName) name domain
        bareEmail' = do
          name <- manyTill anyChar (try (char '@'))
          domain <- many1 (noneOf "<>")
          return $ strip <$> (pack name, pack domain)
        bareEmail = do
          (name, domain) <- bareEmail'
          return $ EmailAddress (name <> "@" <> domain) name domain

sortByGroupSize :: Algo.Comparison (Grouping a)
sortByGroupSize (Grouping _ a) (Grouping _ b) =
  if diff < 0 then GT
  else if diff > 0 then LT
  else EQ
  where diff = Vec.length a - Vec.length b

sortByAgeAndGroupSize :: Algo.Comparison (Grouping Message)
sortByAgeAndGroupSize ga@(Grouping _ a) gb@(Grouping _ b) =
  if diff < 0 then GT
  else if diff > 0 then LT
  else sortByGroupSize ga gb
  where diff = oldest a - oldest b
        oldest :: Vector Message -> Integer
        oldest v = Vec.minimum (dateSent <$> v)

toMessage :: [M.Message] -> Vector Message
toMessage messages = Vec.fromList $ gmailToMessage <$> messages

inboxEndpoint :: [M.Message] -> IO (Vector (Grouping Message))
inboxEndpoint messages = do
  let groups = groupBySender $ toMessage messages
      sorted = Vec.modify (Algo.sortBy sortByAgeAndGroupSize) groups
  return sorted

gmailToMessage :: M.Message -> Message
gmailToMessage gmail = Message
  { messageId = fromStrict $ M._id gmail
  , subject = fromStrict $ fromMaybe "<no subject>" $ M.extractHeader gmail "Subject"
  , snippet = fromStrict $ M._snippet gmail
  , from = case parseEmailAddress emailText of
      Left err -> EmailAddress emailText "" (pack $ show err)
      Right e -> e
  , body = fromStrict . fromMaybe "<no body>" . M._data . M._body . M._payload $ gmail
  , headers = (\h -> Header (fromStrict $ M._name h) (fromStrict $ M._value h)) <$> headers
  , dateSent = case decimal (M._internalDate gmail) of
      Left _ -> -1
      Right (val, _) -> val
  }
  where emailText = fromStrict $ fromMaybe "?" (M.extractHeader gmail "From")
        headers = M._headers . M._payload $ gmail
