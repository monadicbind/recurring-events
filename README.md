# Recurring Events Library

   This library gives the opportunity to the users to schedule events / reminders in a timely manner. It attempts to provide relevant apis / dsl to help define the event and when it should occur.

## Prerequisites

1 . Make sure `stack` is installed, on a `mac` this could be done using `brew install stack`
2. Unzip the folder and run the command `stack build` and later run `stack test` , after making sure you are in a terminal and in the folder of the source.

## Intent
  The intent of building this library in haskell is to show case how relevant it is to think and implement such problems in languages that are lazy and have a functional bent. If the solution provide could showcase the solution in the light of typesafety and extensibility, that would be a win for programming in such kind of languages.

## Design Thought Process

As part of the design, there are three core ADT's Schedule , RecurringEvent and RepeatSchedule. The RepeatSchedule define when should an event occur. A RecurringEvent has a name , a start date , an end date and the RepeatSchedule. A Schedule is something that has a RecurringEvent.

The code uses GADTs for describing the ADTs and also uses three type classes to mandate the kind of methods that are required so that the 3 ADTs defined above could work with each other.

There is a focus on extensibility and it is demonstrated in the code in the file `EveryNTimes.hs`. This file is an example of how a user could add a new repeat schedule and make it work with the existing typeclasses of `RecurringEventHelper` and `Scheduler`.
