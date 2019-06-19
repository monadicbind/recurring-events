{-# LANGUAGE GADTs #-}

module RepeatSchedule where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock

data DayOfWeek
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Enum, Show, Eq)

data RepeatSchedule where
        WeekDayOfMonth :: [Int] -> DayOfWeek -> RepeatSchedule
        DayOfMonth :: Int -> RepeatSchedule
        MonthNumber :: Int -> RepeatSchedule
        YearNumber :: Int -> RepeatSchedule
        OrCombinator ::
            (RepeatScheduleHelper a, RepeatScheduleHelper b) =>
            a -> b -> RepeatSchedule
        AndCombinator ::
            (RepeatScheduleHelper a, RepeatScheduleHelper b) =>
            a -> b -> RepeatSchedule

class RepeatScheduleHelper a where
  scheduleLogic :: a -> Day -> Bool

instance RepeatScheduleHelper RepeatSchedule where
  scheduleLogic (MonthNumber monNum) day =
    let internalDay = getMonthOfDateWithWeekAndOrdinal day
    in month internalDay == monNum
  scheduleLogic (YearNumber yrNum) day =
    let internalDay = getMonthOfDateWithWeekAndOrdinal day
    in year internalDay == toInteger yrNum
  scheduleLogic (AndCombinator one another) day = scheduleLogic one day && scheduleLogic another day
  scheduleLogic (WeekDayOfMonth wkNum dow) day =
    let internalDay = getMonthOfDateWithWeekAndOrdinal day
    in (fromEnum dow == dOW internalDay && ((mod (week internalDay) 4 + 1) `elem` wkNum))
  scheduleLogic (DayOfMonth date) someDay =
    let internalDay = getMonthOfDateWithWeekAndOrdinal someDay
    in (day internalDay == date)
  scheduleLogic (OrCombinator this that) day = scheduleLogic this day || scheduleLogic that day

data InternalDay = InternalDay
  { year :: Integer
  , month :: Int
  , week :: Int
  , day :: Int
  , dOW :: Int
  } deriving (Show, Eq)

getMonthOfDateWithWeekAndOrdinal :: Day -> InternalDay
getMonthOfDateWithWeekAndOrdinal day =
  let (year, month, dd) = toGregorian day
  in InternalDay year month (fst (sundayStartWeek day)) dd (snd (sundayStartWeek day))

getDayFromTupleList :: InternalDay -> Day
getDayFromTupleList internalDay =
  fromSundayStartWeek (year internalDay) (week internalDay) (dOW internalDay)
