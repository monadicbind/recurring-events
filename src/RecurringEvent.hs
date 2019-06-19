{-# LANGUAGE GADTs #-}

module RecurringEvent where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import RepeatSchedule

data RecurringEvent a where
        RecurringEvent ::
            RepeatScheduleHelper a =>
            String -> Day -> Day -> a -> RecurringEvent a

class RecurringEventHelper a where
  getAllOcc :: a -> [Day]

instance RecurringEventHelper (RecurringEvent a) where
  getAllOcc (RecurringEvent _ startDate endDate repeatSched) =
    filter (scheduleLogic repeatSched) [startDate .. endDate]
