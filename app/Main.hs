module Main where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import EveryNTimes
import RecurringEvent
import RepeatSchedule
import Scheduler

main :: IO ()
main = fmap (show . sundayStartWeek) (return today) >>= putStrLn

today :: Day
today = fromGregorian 2019 06 05

nYearsLaterDate :: Int -> Day -> Day
nYearsLaterDate n = addDays (fromIntegral (n * 365))

quarterly = AndCombinator EveryQuarter (DayOfMonth 1)

scheduleQuartelyEvent =
  Schedule (RecurringEvent "quarterly" (fromGregorian 2019 1 1) (fromGregorian 2020 1 1) quarterly)

getQuartAllOcc = getAllOccurences scheduleQuartelyEvent
