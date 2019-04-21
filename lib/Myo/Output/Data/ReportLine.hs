module Myo.Output.Data.ReportLine where

import Myo.Output.Data.OutputEvent (EventIndex)

data ReportLine =
  ReportLine {
    _event :: EventIndex,
    _text :: Text
  }
  deriving (Eq, Show)
