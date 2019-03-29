module Myo.Output.Data.ParseReport where

import Myo.Output.Data.OutputEvent (OutputEvent)
import Myo.Output.Data.ReportLine (ReportLine)

data ParseReport =
  ParseReport {
    _events :: [OutputEvent],
    _lines :: [ReportLine]
  }
  deriving (Eq, Show)
