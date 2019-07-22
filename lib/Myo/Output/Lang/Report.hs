module Myo.Output.Lang.Report where

import Control.Lens (ifolded, view, withIndex)
import Data.Vector (Vector)
import Data.Vector.Lens (toVectorOf)

import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
import Myo.Output.Data.OutputEvent (LangOutputEvent)
import qualified Myo.Output.Data.OutputEvent as LangOutputEvent (event)
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

parsedOutputCons ::
  (LangOutputEvent a -> Vector Text) ->
  Vector (LangOutputEvent a) ->
  ParseReport EventIndex.Relative
parsedOutputCons format events =
  ParseReport events' lines'
  where
    events' =
      view LangOutputEvent.event <$> events
    lines' :: Vector (ReportLine EventIndex.Relative)
    lines' =
      uncurry line' =<< zipWithIndex events
    line' index event =
      ReportLine (EventIndex.Relative (fromIntegral index)) <$> format event
    zipWithIndex =
      toVectorOf (ifolded . withIndex)
