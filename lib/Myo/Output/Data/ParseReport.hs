module Myo.Output.Data.ParseReport where

import Myo.Output.Data.OutputEvent (OutputEvent)
import Myo.Output.Data.ReportLine (ReportLine)

data ParseReport =
  ParseReport {
    _events :: [OutputEvent],
    _lines :: [ReportLine]
  }
  deriving (Eq, Show)

instance Semigroup ParseReport where
  (ParseReport e1 l1) <> (ParseReport e2 l2) = ParseReport (e1 <> e2) (l1 <> l2)

instance Monoid ParseReport where
  mempty = ParseReport mempty mempty

noEventsInReport :: ParseReport -> Bool
noEventsInReport (ParseReport e _) =
  null e
