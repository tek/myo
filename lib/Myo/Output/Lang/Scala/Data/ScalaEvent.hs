module Myo.Output.Lang.Scala.Data.ScalaEvent where

import Data.Text (Text)

import Myo.Output.Data.Location (Location)

data EventType =
  Error
  |
  Warning
  deriving (Eq, Show)

data ScalaEvent =
  ScalaEvent {
     _location :: Location,
     _eventType :: EventType,
     _errorMessage :: Text,
     _errorInfo :: [Text],
     _codeIndent :: Int,
     _code :: Text
  }
  deriving (Eq, Show)
