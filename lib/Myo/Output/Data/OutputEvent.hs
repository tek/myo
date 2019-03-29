module Myo.Output.Data.OutputEvent where

import Data.DeepLenses (deepLenses)
import Data.List.NonEmpty (NonEmpty)
import Prelude hiding (lines)

import Myo.Output.Data.Location (Location)
import Myo.Output.Data.OutputLine (OutputLine)

data OutputEvent =
  OutputEvent {
     _location :: Maybe Location,
     _level :: Int
  }
  deriving (Eq, Show)
