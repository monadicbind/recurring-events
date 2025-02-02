{-# LANGUAGE GADTs #-}

module RecurringEvent
  ( createRecurringEvent
  , eventName
  , startDate
  , endDate
  , repeatSchedule
  , RecurringEventHelper(..)
  , RecurringEvent
  ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import RepeatSchedule

data RecurringEvent a where
        RecurringEvent ::
            RepeatScheduleHelper a =>
            String -> a -> Day -> Day -> RecurringEvent a

{-
  This is a way to only expose methods that helps one to
  construct the ADT and not expose the details behind the
  ADT. In Haskell , this type of initialization is called
  as Smart Constructors.
-}
createRecurringEvent
  :: RepeatScheduleHelper a
  => String -> a -> Day -> Day -> RecurringEvent a
createRecurringEvent = RecurringEvent

eventName
  :: RepeatScheduleHelper a
  => RecurringEvent a -> String
eventName (RecurringEvent name _ _ _) = name

startDate
  :: RepeatScheduleHelper a
  => RecurringEvent a -> Day
startDate (RecurringEvent _ _ stDate _) = stDate

endDate
  :: RepeatScheduleHelper a
  => RecurringEvent a -> Day
endDate (RecurringEvent _ _ _ endDate) = endDate

repeatSchedule
  :: RepeatScheduleHelper a
  => RecurringEvent a -> a
repeatSchedule (RecurringEvent _ a _ _) = a

class RecurringEventHelper a where
  getAllOcc :: a -> [Day]

instance RecurringEventHelper (RecurringEvent a) where
  getAllOcc (RecurringEvent _ repeatSched startDate endDate) =
    filter (scheduleLogic repeatSched) [startDate .. endDate]
