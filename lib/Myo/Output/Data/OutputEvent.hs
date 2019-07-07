module Myo.Output.Data.OutputEvent where

import Myo.Output.Data.Location (Location)

data OutputEvent =
  OutputEvent {
     _location :: Maybe Location,
     _level :: Int
  }
  deriving (Eq, Show)

data LangOutputEvent a =
  LangOutputEvent {
    _event :: OutputEvent,
    _langData :: a
  }
  deriving (Eq, Show)

makeClassy ''LangOutputEvent
