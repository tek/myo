module Myo.Output.Lang.Scala.Data.ScalaEvent where

import Myo.Output.Data.Location (Location)

data EventType =
  Error
  |
  Warning
  deriving stock (Eq, Show)

data ScalaEvent =
  ScalaEvent {
     _location :: Location,
     _eventType :: EventType,
     _errorMessage :: Text,
     _errorInfo :: [Text],
     _codeIndent :: Int,
     _code :: Text
  }
  deriving stock (Eq, Show)
