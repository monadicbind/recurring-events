{-# LANGUAGE GADTs #-}

module Scheduler where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import RecurringEvent
import RepeatSchedule

someFunc :: IO ()
someFunc = putStr "someFunc"

todaysDate :: IO UTCTime
todaysDate = getCurrentTime

scheduleARecurringEvent
  :: RecurringEventHelper a
  => a -> Schedule a
scheduleARecurringEvent = Schedule

data Schedule a where
        Schedule :: RecurringEventHelper a => a -> Schedule a

class RecurringEventHelper a =>
      Scheduler a where
  startDate :: Schedule a -> Day
  endDate :: Schedule a -> Day
  getOccurences :: Schedule a -> Int -> [Day]
  getAllOccurences :: Schedule a -> [Day]

instance Scheduler (RecurringEvent a) where
  startDate (Schedule (RecurringEvent _ stDate _ _)) = stDate
  endDate (Schedule (RecurringEvent _ _ eDate _)) = eDate
  getAllOccurences (Schedule re) = getAllOcc re
  getOccurences schedule n = take n (getAllOccurences schedule)
