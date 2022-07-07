module Myo.Output.Lang.Report where

import qualified Data.Vector as Vector
import Data.Vector (Vector)

import qualified Myo.Output.Data.EventIndex as EventIndex (Relative (Relative))
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEvent (OutputEvent))
import Myo.Output.Data.OutputEvents (OutputEvents (OutputEvents))
import Myo.Output.Data.ReportLine (ReportLine (ReportLine))

parsedOutputCons ::
  ∀ a .
  (LangOutputEvent a -> Vector Text) ->
  Vector (LangOutputEvent a) ->
  OutputEvents
parsedOutputCons format events =
  OutputEvents (Vector.imap outputEvent events)
  where
    outputEvent :: Int -> LangOutputEvent a -> OutputEvent
    outputEvent index e@(LangOutputEvent meta _) =
      OutputEvent meta (lines' index e)
    lines' index event =
      ReportLine (EventIndex.Relative (fromIntegral index)) <$> format event
