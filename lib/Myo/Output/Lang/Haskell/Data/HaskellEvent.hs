module Myo.Output.Lang.Haskell.Data.HaskellEvent where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

import Myo.Output.Data.Location (Location)

data EventType =
  Error
  |
  Warning
  |
  RuntimeError
  |
  Patterns
  deriving (Eq, Show)

data HaskellEvent =
  HaskellEvent {
     _location :: Location,
     _eventType :: EventType,
     _messages :: NonEmpty Text
  }
  deriving (Eq, Show)
