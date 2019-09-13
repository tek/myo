module Myo.Output.Data.OutputEvent where

import Data.Vector (Vector)
import Prelude hiding (lines)

import qualified Myo.Output.Data.EventIndex as EventIndex (Relative)
import Myo.Output.Data.Location (Location)
import Myo.Output.Data.ReportLine (ReportLine)

data OutputEventMeta =
  OutputEventMeta {
     _location :: Maybe Location,
     _level :: Int
  }
  deriving (Eq, Show)

makeClassy ''OutputEventMeta

data OutputEvent =
  OutputEvent {
     _meta :: OutputEventMeta,
    _lines :: Vector (ReportLine EventIndex.Relative)
  }
  deriving (Eq, Show)

makeClassy ''OutputEvent

data LangOutputEvent a =
  LangOutputEvent {
    _event :: OutputEventMeta,
    _langData :: a
  }
  deriving (Eq, Show)

makeClassy ''LangOutputEvent
