module EveryNTimes where

import Data.Time.Calendar
import RepeatSchedule

{-|
  This is how one can add new Repeat Schedules without
  affecting the Library Functions that are already provided.
-}
data EveryNTimes =
  EveryQuarter
  -- | EveryXDays { xDays :: Int
     --         ,  initDay :: Day}
  deriving (Show)

instance RepeatScheduleHelper EveryNTimes where
  scheduleLogic EveryQuarter day =
    let internalDay = getMonthOfDateWithWeekAndOrdinal day
    in month internalDay `mod` 3 == 0
--  scheduleLogic (EveryXDays days initDay) day =
  --  let initInternalDay = getMonthOfDateWithWeekAndOrdinal initDay
    --    compareToInternalDay = getMonthOfDateWithWeekAndOrdinal day
