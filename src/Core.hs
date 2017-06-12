{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Core
  ( incoming
  , remindMe
  , toTask
  , task
  , tasks
  , prioritizeTask
  , archive
  , seconds
  , minutes
  , hours
  , prioritize
  , Incoming(..)
  , Reminder
  , Task(..)
  , Status(..)
  , TimeLog
  , TimeTrackEvent(..)
  , Timestamp
  , Prioritized
  , PrioritizedF
  , Archived
  , TimeSpan
  ) where

import Data.List (splitAt)

data Incoming a = Incoming a
  deriving Functor

data Reminder a = Reminder TimeSpan a
  deriving Functor

data Task a = Task Status TimeLog a
  deriving Functor

data Status = Created | InProgress | Done | Cancelled
  deriving Show

data TimeLog = TimeLog [TimeTrackEvent]
  deriving Show

instance Monoid TimeLog where
  mempty = TimeLog []
  mappend (TimeLog a) (TimeLog b) = TimeLog $ mappend a b

data TimeTrackEvent = ClockIn Timestamp | ClockOut Timestamp
  deriving Show

type Timestamp = Int

newtype PrioritizedF a = Priority a
  deriving Functor

type Prioritized a = PrioritizedF [Task a]

tasks :: PrioritizedF a -> a
tasks (Priority a) = a

instance Monoid a => Monoid (PrioritizedF a) where
  mempty = Priority mempty
  mappend (Priority a) (Priority b) = Priority $ mappend a b

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

task :: a -> Task a
task a = Task Created mempty a

remindMe :: a -> TimeSpan -> Reminder a
remindMe a ts = Reminder ts a

archive :: Incoming a -> Archived a
archive (Incoming a) = Archived a

prioritizeTask :: Functor f => a -> Int -> f [Task a] -> f [Task a]
prioritizeTask = prioritize task

prioritize :: Functor f => (a -> b) -> a -> Int -> f [b] -> f [b]
prioritize f msg loc p = fmap ins p
  where ins xs = let (smaller, larger) = splitAt loc xs
                 in smaller ++ [f msg] ++ larger
