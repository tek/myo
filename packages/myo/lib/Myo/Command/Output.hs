module Myo.Command.Output where

import GHC.Natural (minusNaturalMaybe)
import Ribosome (Handler, Rpc, RpcError, Scratch, Settings, mapReport)
import Ribosome.Host.Data.Report (resumeReports)
import qualified Ribosome.Settings as Settings

import qualified Myo.Effect.Outputs as Outputs
import Myo.Effect.Outputs (Outputs)
import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.OutputError (OutputError (NoEvents))
import Myo.Output.Data.ParseReport (ParseReport (ParseReport), noEventsInReport)
import Myo.Output.ParseReport (
  compileReport,
  currentEvents,
  currentOutputCommandDesc,
  currentSyntax,
  cycleIndex,
  navigateToCurrentEvent,
  renderReport,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

initialRenderReport ::
  Members [Outputs, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  ParseReport ->
  Sem r ()
initialRenderReport report@(ParseReport events _) = do
  jumpFirst <- Settings.get Settings.outputSelectFirst
  Outputs.setCurrentEvent (EventIndex.Absolute (first' jumpFirst))
  syntax <- currentSyntax
  renderReport report syntax
  navigateToCurrentEvent =<< Settings.get Settings.outputAutoJump
  where
    first' jumpFirst =
      if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)

compileAndRenderReport ::
  Members [Outputs, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  Sem r ()
compileAndRenderReport = do
  report <- compileReport 0 <$> currentEvents
  desc <- currentOutputCommandDesc
  when (noEventsInReport report) (stop (NoEvents desc))
  Outputs.setCurrentReport report
  initialRenderReport report

cycleAndNavigate ::
  Members [Outputs, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Int ->
  Sem r ()
cycleAndNavigate offset =
  whenM (cycleIndex offset) (navigateToCurrentEvent True)

myoPrev ::
  Members [Outputs, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoPrev =
  resumeReports @[Scratch, Rpc] @[_, _] $ mapReport do
    cycleAndNavigate (-1)

myoNext ::
  Members [Outputs, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoNext =
  resumeReports @[Scratch, Rpc] @[_, _] $ mapReport do
    cycleAndNavigate 1
