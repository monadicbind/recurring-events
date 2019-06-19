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
  encodeEvent (Event (T.pack "Stand-Up") (T.pack "daily")) `shouldBe`
  T.pack "{\"name\":\"Stand-Up\",\"repeat\":\"daily\"}"
