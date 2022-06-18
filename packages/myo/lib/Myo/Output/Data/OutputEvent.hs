module Myo.Output.Data.OutputEvent where

import Data.Vector (Vector)
import Prelude hiding (lines)

import qualified Myo.Output.Data.EventIndex as EventIndex (Relative)
import Myo.Output.Data.Location (Location)
import Myo.Output.Data.ReportLine (ReportLine)

data OutputEventMeta =
  OutputEventMeta {
    location :: Maybe Location,
    level :: Int
  }
  deriving stock (Eq, Show)

data OutputEvent =
  OutputEvent {
    meta :: OutputEventMeta,
    lines :: Vector (ReportLine EventIndex.Relative)
  }
  deriving stock (Eq, Show)

data LangOutputEvent a =
  LangOutputEvent {
    event :: OutputEventMeta,
    langData :: a
  }
  deriving stock (Eq, Show)
