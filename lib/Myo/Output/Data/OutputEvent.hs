module Myo.Output.Data.OutputEvent where

import Prelude hiding (lines)

import Myo.Output.Data.Location (Location)

data OutputEvent =
  OutputEvent {
     _location :: Maybe Location,
     _level :: Int
  }
  deriving (Eq, Show)
