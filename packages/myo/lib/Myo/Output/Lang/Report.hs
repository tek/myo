module Myo.Output.Lang.Report where

-- import Control.Lens (ifolded, withIndex)
-- import Data.Vector (Vector)
-- import Data.Vector.Lens (toVectorOf)

-- import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
-- import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEvent(OutputEvent))
-- import Myo.Output.Data.OutputEvents (OutputEvents(OutputEvents))
-- import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

-- parsedOutputCons ::
--   ∀ a .
--   (LangOutputEvent a -> Vector Text) ->
--   Vector (LangOutputEvent a) ->
--   OutputEvents
-- parsedOutputCons format events =
--   OutputEvents (uncurry outputEvent <$> zipWithIndex events)
--   where
--     outputEvent :: Int -> LangOutputEvent a -> OutputEvent
--     outputEvent index e@(LangOutputEvent meta _) =
--       OutputEvent meta (lines' index e)
--     lines' index event =
--       ReportLine (EventIndex.Relative (fromIntegral index)) <$> format event
--     zipWithIndex =
--       toVectorOf (ifolded . withIndex)
