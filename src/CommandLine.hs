{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

module CommandLine where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

type Grouping a b = Eq b => a -> b

data Group b a = Group
  { _key :: b
  , _values :: a
  } deriving Functor

group :: Ord b => [a] -> Grouping a b -> Map.Map b (Group b [a])
group xs g = foldl' combine Map.empty xs
  where combine m t = Map.adjust (update t) (g t) m

update :: Functor f => a -> f [a]-> f [a]
update t group = fmap ((:) t) group
