module Myo.Output.Data.ParseReport where

import Data.Vector (Vector)
import Prelude hiding (lines)

import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute)
import Myo.Output.Data.OutputEvent (OutputEventMeta)
import Myo.Output.Data.ReportLine (ReportLine)

data ParseReport =
  ParseReport {
    _events :: Vector OutputEventMeta,
    _lines :: Vector (ReportLine EventIndex.Absolute)
  }
  deriving (Eq, Show)

makeClassy ''ParseReport

noEventsInReport :: ParseReport -> Bool
noEventsInReport (ParseReport e _) =
  null e
