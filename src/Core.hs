{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Core
  ( incoming
  , remindMe
  , toTask
  , prioritizeTask
  , archive
  , seconds
  , minutes
  , hours
  , prioritize
  ) where

import Data.List (splitAt)

data Incoming a = Incoming a
  deriving Functor

data Reminder a = Reminder TimeSpan a
  deriving Functor

data Task a = Task Status TimeLog a
  deriving Functor

data Status = Created | InProgress | Done | Cancelled

data TimeLog = TimeLog [TimeTrackEvent]

instance Monoid TimeLog where
  mempty = TimeLog []
  mappend (TimeLog a) (TimeLog b) = TimeLog $ mappend a b

data TimeTrackEvent = ClockIn Timestamp | ClockOut Timestamp

type Timestamp = Int

data Message a = Message a
  deriving Functor

data PrioritizedF a = Priority a
  deriving Functor

type Prioritized a = PrioritizedF [Task a]

data Archived a = Archived a
  deriving Functor

data TimeSpan = HMS Int

seconds :: Int -> TimeSpan
seconds = HMS

minutes :: Int -> TimeSpan
minutes m = seconds (m * 60)

hours :: Int -> TimeSpan
hours h = minutes (h * 60)

incoming :: a -> Incoming a
incoming = Incoming

toTask :: Incoming a -> Task a
toTask (Incoming a) = Task Created mempty a

remindMe :: a -> TimeSpan -> Reminder a
remindMe a ts = Reminder ts a

archive :: Incoming a -> Archived a
archive (Incoming a) = Archived a

prioritizeTask :: Functor f => Incoming a -> Int -> f [Task a] -> f [Task a]
prioritizeTask = prioritize toTask

prioritize :: Functor f => (a -> b) -> a -> Int -> f [b] -> f [b]
prioritize f msg loc p = fmap ins p
  where ins xs = let (smaller, larger) = splitAt loc xs
                 in smaller ++ [f msg] ++ larger
