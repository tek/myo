module Myo.Command.Output where

import GHC.Natural (minusNaturalMaybe)
import Ribosome (Handler, Rpc, RpcError, Scratch, Settings, mapReport)
import Ribosome.Host.Data.Report (resumeReports)
import qualified Ribosome.Settings as Settings

import qualified Myo.Command.Commands as Commands
import Myo.Command.Data.CommandError (CommandError)
import Myo.Data.CommandQuery (queryIdH)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.OutputError (OutputError (NoEvents))
import Myo.Output.Data.ParseReport (ParseReport (ParseReport), noEventsInReport)
import Myo.Output.ParseReport (
  compileReport,
  currentEvents,
  currentOutputCommand,
  currentSyntax,
  cycleIndex,
  navigateToCurrentEvent,
  renderReport,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

initialRenderReport ::
  Members [Commands, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  ParseReport ->
  Sem r ()
initialRenderReport report@(ParseReport events _) = do
  jumpFirst <- Settings.get Settings.outputSelectFirst
  Commands.setCurrentEvent (EventIndex.Absolute (first' jumpFirst))
  syntax <- currentSyntax
  renderReport report syntax
  navigateToCurrentEvent =<< Settings.get Settings.outputAutoJump
  where
    first' jumpFirst =
      if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)

compileAndRenderReport ::
  Members [Commands, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  Sem r ()
compileAndRenderReport = do
  report <- compileReport 0 <$> currentEvents
  ident <- currentOutputCommand
  when (noEventsInReport report) (stop . NoEvents =<< Commands.describe (queryIdH ident))
  Commands.setCurrentReport report
  initialRenderReport report

cycleAndNavigate ::
  Members [Commands, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Int ->
  Sem r ()
cycleAndNavigate offset =
  whenM (cycleIndex offset) (navigateToCurrentEvent True)

myoPrev ::
  Members [Commands !! CommandError, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoPrev =
  resumeReports @[Scratch, Rpc, Commands] @[_, _, _] $ mapReport do
    cycleAndNavigate (-1)

myoNext ::
  Members [Commands !! CommandError, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoNext =
  resumeReports @[Scratch, Rpc, Commands] @[_, _, _] $ mapReport do
    cycleAndNavigate 1
