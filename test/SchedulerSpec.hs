module SchedulerSpec
  ( spec
  ) where

import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import EveryNTimes
import RecurringEvent
import RepeatSchedule
import Scheduler

import Test.Hspec

today :: Day
today = fromGregorian 2019 06 05

nYearsLaterDate :: Int -> Day -> Day
nYearsLaterDate n = addDays (fromIntegral (n * 365))

standUpEvent :: (RecurringEvent RepeatSchedule)
standUpEvent =
  createRecurringEvent
    "standup-every-month"
    today
    (nYearsLaterDate 3 today)
    (WeekDayOfMonth [1] Monday)

billPayEvent :: (RecurringEvent RepeatSchedule)
billPayEvent = createRecurringEvent "bill-pay" today (nYearsLaterDate 5 today) (DayOfMonth 28)

spec :: Spec
spec =
  describe "Scheduler" $ do
    describe "startDate" $ do
      it "with a standup event returns the startDate" $ do
        let standUpSchedule = scheduleARecurringEvent standUpEvent
        Scheduler.startDate standUpSchedule `shouldBe` today
      it "with a billpay event returns the startDate " $ do
        let billPaySchedule = scheduleARecurringEvent standUpEvent
        Scheduler.startDate billPaySchedule `shouldBe` today
      it "getMonthOfDateWithWeekAndOrdinal and getDayFromTuple list are inverse of each other " $ do
        let anyDate = fromGregorian 2021 02 29
        let todayInternalDate = getMonthOfDateWithWeekAndOrdinal anyDate
        let todaysDay = getDayFromTupleList todayInternalDate
        todaysDay `shouldBe` anyDate
    describe "get occurences for recurring events" $ do
      it "with a standup event for every monday of a month" $ do
        let aDate = fromGregorian 2020 1 1
        let anotherDate = addDays 30 aDate
        let standUp =
              createRecurringEvent "standup" aDate anotherDate (WeekDayOfMonth [1, 2, 3, 4] Monday)
        let scheduleStandUp = Schedule standUp
        length (getOccurences scheduleStandUp 10) `shouldBe` 4
      it "with standup event for every monday and tuesday of a month" $ do
        let aDate = fromGregorian 2020 1 1
        let anotherDate = addDays 30 aDate
        let standUpEveryMonday = WeekDayOfMonth [1, 2, 3, 4] Monday
        let standUpEveryTuesday = WeekDayOfMonth [1, 2, 3, 4] Tuesday
        let standUp =
              createRecurringEvent
                "standUp"
                aDate
                anotherDate
                (OrCombinator standUpEveryMonday standUpEveryTuesday)
        let scheduleStandUp = scheduleARecurringEvent standUp
        length (getOccurences scheduleStandUp 10) `shouldBe` 8
      it "with anniversary of 2nd September" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = fromGregorian 3000 1 1
        let dayOfMonth = DayOfMonth 2
        let monthNumber = MonthNumber 9
        let event = AndCombinator dayOfMonth monthNumber
        let anniversary = createRecurringEvent "anniversary" aDate anotherDate event
        let scheduleAnniversary = scheduleARecurringEvent anniversary
        length (getOccurences scheduleAnniversary 10) `shouldBe` 10
        map ((\(_, b, _) -> b) . toGregorian) (getOccurences scheduleAnniversary 10) `shouldBe`
          replicate 10 9
      it "with a non recurring event which hasnt passed away" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = fromGregorian 3000 1 1
        let dayOfMonth = DayOfMonth 2
        let monthNumber = MonthNumber 9
        let yearNumber = YearNumber 2019
        let event = AndCombinator monthNumber dayOfMonth
        let dateEvent = AndCombinator yearNumber event
        let nonRecurring = createRecurringEvent "non-recurring" aDate anotherDate dateEvent
        let scheduleNonrecurring = scheduleARecurringEvent nonRecurring
        length (getOccurences scheduleNonrecurring 10) `shouldBe` 1
        getOccurences scheduleNonrecurring 10 `shouldBe` [fromGregorian 2019 9 2]
      it "with a non recurring event which has passed away" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = fromGregorian 3000 1 1
        let dayOfMonth = DayOfMonth 2
        let monthNumber = MonthNumber 9
        let yearNumber = YearNumber 2018
        let event = AndCombinator monthNumber dayOfMonth
        let dateEvent = AndCombinator yearNumber event
        let nonRecurring = createRecurringEvent "non-recurring" aDate anotherDate dateEvent
        let scheduleNonrecurring = scheduleARecurringEvent nonRecurring
        length (getOccurences scheduleNonrecurring 10) `shouldBe` 0
      it "remind every First (1) of every quarter for a year" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = fromGregorian 2020 1 1
        let firstQuarter = AndCombinator (MonthNumber 3) (DayOfMonth 1)
        let secondQuarter = AndCombinator (MonthNumber 6) (DayOfMonth 1)
        let thirdQuarter = AndCombinator (MonthNumber 9) (DayOfMonth 1)
        let fourthQuarter = AndCombinator (MonthNumber 12) (DayOfMonth 1)
        let repeatSchedule =
              OrCombinator
                firstQuarter
                (OrCombinator secondQuarter (OrCombinator thirdQuarter fourthQuarter))
        let event = createRecurringEvent "1st every Quarter" aDate anotherDate repeatSchedule
        let scheduleEvent = scheduleARecurringEvent event
        length (getOccurences scheduleEvent 10) `shouldBe` 4
        getAllOccurences scheduleEvent `shouldBe`
          [ fromGregorian 2019 3 1
          , fromGregorian 2019 6 1
          , fromGregorian 2019 9 1
          , fromGregorian 2019 12 1
          ]
      it "every alternate day in 30 days" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = addDays 30 aDate
        let alternateEvent =
              createRecurringEvent "alternate-day" aDate anotherDate (EveryXDays (2 :: Int) aDate)
        let scheduledEvent = scheduleARecurringEvent alternateEvent
        length (getAllOccurences scheduledEvent) `shouldBe` 16
      it "every third day in 30 days" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = addDays 30 aDate
        let alternateEvent =
              createRecurringEvent "alternate-day" aDate anotherDate (EveryXDays (3 :: Int) aDate)
        let scheduledEvent = scheduleARecurringEvent alternateEvent
        length (getAllOccurences scheduledEvent) `shouldBe` 11
      it "every day in 30 days" $ do
        let aDate = fromGregorian 2019 1 1
        let anotherDate = addDays 30 aDate
        let alternateEvent =
              createRecurringEvent
                "everyday-standup"
                aDate
                anotherDate
                (EveryXDays (1 :: Int) aDate)
        let scheduledEvent = scheduleARecurringEvent alternateEvent
        length (getAllOccurences scheduledEvent) `shouldBe` 31
