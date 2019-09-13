module Myo.Command.Output where

import Control.Monad (when)
import Control.Monad.DeepState (MonadDeepState)
import GHC.Natural (minusNaturalMaybe)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (killScratchByName)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as OutputState (currentEvent, report)
import Myo.Command.History (displayNameByIdent)
import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute(Absolute))
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport), noEventsInReport)
import Myo.Output.ParseReport (
  compileReport,
  currentEvents,
  currentOutputCommand,
  currentReport,
  currentSyntax,
  cycleIndex,
  navigateToCurrentEvent,
  outputMainWindow,
  outputWindow,
  renderReport,
  scratchName,
  selectCurrentLineEventFrom,
  setOutput,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

initialRenderReport ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  ParseReport ->
  m ()
initialRenderReport report@(ParseReport events _) = do
  jumpFirst <- setting Settings.outputSelectFirst
  setOutput OutputState.currentEvent (EventIndex.Absolute (first' jumpFirst))
  syntax <- currentSyntax
  renderReport report syntax
  navigateToCurrentEvent =<< setting Settings.outputAutoJump
  where
    first' jumpFirst =
      if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)

compileAndRenderReport ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  m ()
compileAndRenderReport = do
  report <- compileReport 0 <$> currentEvents
  ident <- currentOutputCommand
  when (noEventsInReport report) (throwHoist . NoEvents =<< displayNameByIdent ident)
  setOutput OutputState.report (Just report)
  initialRenderReport report

outputQuit ::
  MonadRibo m =>
  NvimE e m =>
  m ()
outputQuit =
  killScratchByName scratchName

outputSelect ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  m ()
outputSelect = do
  mainWindow <- outputMainWindow
  window <- outputWindow
  report <- currentReport
  selectCurrentLineEventFrom report window mainWindow

cycleAndNavigate ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  Int ->
  m ()
cycleAndNavigate offset =
  whenM (cycleIndex offset) (navigateToCurrentEvent True)

myoPrev ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  m ()
myoPrev =
  cycleAndNavigate (-1)

myoNext ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  m ()
myoNext =
  cycleAndNavigate 1
