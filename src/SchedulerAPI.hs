{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module SchedulerAPI where

import Data.Aeson
import Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics

data Event = Event
  { name :: T.Text
  , repeat :: T.Text
  } deriving (Generic, Show)

instance ToJSON Event

instance FromJSON Event

encodeEvent :: Event -> T.Text
encodeEvent = decodeUtf8 . encode
