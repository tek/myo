module Myo.Output.Data.OutputEvent where

import Data.Default (Default)

import Myo.Output.Data.Location (Location)

newtype EventIndex =
  EventIndex Int
  deriving (Eq, Show, Default)

data OutputEvent =
  OutputEvent {
     _location :: Maybe Location,
     _level :: Int
  }
  deriving (Eq, Show)
