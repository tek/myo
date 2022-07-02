module Myo.Command.Output where

import GHC.Natural (minusNaturalMaybe)
import Ribosome (Handler, Rpc, RpcError, Scratch, Settings, mapHandlerError, resumeHandlerError)
import qualified Ribosome.Scratch as Scratch
import qualified Ribosome.Settings as Settings

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.History (displayNameByIdent)
import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.OutputError (OutputError (NoEvents))
import Myo.Output.Data.ParseReport (ParseReport (ParseReport), noEventsInReport)
import Myo.Output.ParseReport (
  compileReport,
  currentEvents,
  currentOutputCommand,
  currentReport,
  currentSyntax,
  cycleIndex,
  modifyOutput,
  navigateToCurrentEvent,
  outputMainWindow,
  outputWindow,
  renderReport,
  scratchId,
  selectCurrentLineEventFrom,
  setOutput,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

initialRenderReport ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  ParseReport ->
  Sem r ()
initialRenderReport report@(ParseReport events _) = do
  jumpFirst <- Settings.get Settings.outputSelectFirst
  modifyOutput (#currentEvent .~ EventIndex.Absolute (first' jumpFirst))
  syntax <- currentSyntax
  renderReport report syntax
  navigateToCurrentEvent =<< Settings.get Settings.outputAutoJump
  where
    first' jumpFirst =
      if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)

compileAndRenderReport ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Settings, Stop OutputError, Embed IO] r =>
  Sem r ()
compileAndRenderReport = do
  report <- compileReport 0 <$> currentEvents
  ident <- currentOutputCommand
  when (noEventsInReport report) (stop . NoEvents =<< displayNameByIdent ident)
  setOutput #report (Just report)
  initialRenderReport report

myoOutputQuit ::
  Member (Scratch !! RpcError) r =>
  Handler r ()
myoOutputQuit =
  resumeHandlerError (Scratch.kill scratchId)

outputSelect ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Sem r ()
outputSelect = do
  mainWindow <- outputMainWindow
  window <- outputWindow
  report <- currentReport
  selectCurrentLineEventFrom report window mainWindow

cycleAndNavigate ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Int ->
  Sem r ()
cycleAndNavigate offset =
  whenM (cycleIndex offset) (navigateToCurrentEvent True)

myoPrev ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoPrev =
  mapHandlerError do
    cycleAndNavigate (-1)

myoNext ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoNext =
  mapHandlerError do
    cycleAndNavigate 1
