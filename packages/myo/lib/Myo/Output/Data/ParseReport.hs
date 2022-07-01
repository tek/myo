module Myo.Output.Data.ParseReport where

import Data.Vector (Vector)

import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute)
import Myo.Output.Data.OutputEvent (OutputEventMeta)
import Myo.Output.Data.ReportLine (ReportLine)

data ParseReport =
  ParseReport {
    events :: Vector OutputEventMeta,
    lines :: Vector (ReportLine EventIndex.Absolute)
  }
  deriving stock (Eq, Show, Generic)

noEventsInReport :: ParseReport -> Bool
noEventsInReport (ParseReport e _) =
  null e
