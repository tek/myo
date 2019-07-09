module Myo.Output.Data.ParseReport where

import Data.Vector (Vector)
import Prelude hiding (lines)

import Myo.Output.Data.OutputEvent (OutputEvent)
import Myo.Output.Data.ReportLine (ReportLine)

data ParseReport a =
  ParseReport {
    _events :: Vector OutputEvent,
    _lines :: Vector (ReportLine a)
  }
  deriving (Eq, Show)

makeClassy ''ParseReport

instance Semigroup (ParseReport a) where
  (ParseReport e1 l1) <> (ParseReport e2 l2) = ParseReport (e1 <> e2) (l1 <> l2)

instance Monoid (ParseReport a) where
  mempty = ParseReport mempty mempty

noEventsInReport :: ParseReport a -> Bool
noEventsInReport (ParseReport e _) =
  null e
