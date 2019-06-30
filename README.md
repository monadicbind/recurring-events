# Recurring Events Library

   This library gives the opportunity to the users to schedule events / reminders in a timely manner. It attempts to provide relevant apis / dsl to help define the event and when it should occur.

## Prerequisites

* Make sure `stack` is installed, on a `mac` this could be done using
    ```
    brew install stack
    ```
* Unzip the folder and run the command
    ```
    stack build
    ```
* Ensure that you are in the terminal and in the folder of the source.
* Once the libraries are fetched run
    ```
       stack test
     ```

## Intent
  The intent of building this library in haskell is to show case how relevant it is to think and implement such problems in languages that are lazy and have a functional bent. If the solution provide could showcase the solution in the light of typesafety and extensibility, that would be a win for programming in such kind of languages.

## Design Thought Process

When writing the solution in haskell , the contracts are established via TypeClasses and the data structures are ADTs ( Algebraic Data Types).

The three typeclasses that are defined are

* `Scheduler` : This defines the contract that pdf expects one to write.
* `RecurringEventHelper` : This is a helper contract typeclass which defines a function `getAllOcc`. This is the only function that is needed for all the other functions defined in the `Scheduler` typeclass.
* `RepeatScheduleHelper` :This is the typeclass where the logic of matching on the repeat schedule needs to be encoded.

As part of the design, there are three core ADT's

* `Schedule`
* `RecurringEvent`
* `RepeatSchedule`

The RepeatSchedule define when should an event occur. A RecurringEvent has a name , a start date , an end date and the RepeatSchedule. A Schedule is something that has a RecurringEvent.

The code uses GADTs for describing the ADTs and also uses three type classes to mandate the kind of methods that are required so that the 3 ADTs defined above could work with each other.

There is a focus on extensibility and it is demonstrated in the code in the file `EveryNTimes.hs`. This file is an example of how a user could add a new repeat schedule and make it work with the existing typeclasses of `RecurringEventHelper` and `Scheduler`.

## Assumptions

* The end-date of the Recurring Event can be defaulted to some X number of years , so that we dont endup blowing up the `maxBound` of the Integer type of haskell. This assumption can be encoded either in the `RecurringEvent` or in the JSON API while creating the event in question.

## Need to work on

* Adding the JSON API as per the specification. I have started the JSON implementation of the API.
* One could still encode some validation while creating the RepeatSchedule for a Recurring Event, so that outrageous things can be caught at the type level. For example , trying to match a 13nth month in a year. Currently, all the days are checked for this and it could be prevented.
* One could potentially implement `XOR` kind of functionality just like the `And` and the `Or` combinators.

