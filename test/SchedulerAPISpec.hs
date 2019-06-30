{-# LANGUAGE OverloadedStrings #-}

module SchedulerAPISpec
  ( spec
  ) where

import qualified Data.Text.Lazy as T
import SchedulerAPI

import Test.Hspec

spec :: Spec
spec =
  describe "SchedulerAPI" $
  describe " Encode " $
  it "stand-up Event to JSON" $
  encodeEvent (Event "Stand-Up" "daily") `shouldBe` "{\"name\":\"Stand-Up\",\"repeat\":\"daily\"}"
