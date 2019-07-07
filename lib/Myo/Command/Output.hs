module Myo.Command.Output where

import Chiasma.Data.Ident (Ident)
import Control.Monad (when)
import Control.Monad.DeepState (MonadDeepState, setL)
import GHC.Natural (minusNaturalMaybe)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (killScratchByName)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (currentEvent, parseReport)
import Myo.Command.History (displayNameByIdent)
import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute(Absolute))
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport), noEventsInReport)
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.ParseReport (
  compileReport,
  currentReport,
  cycleIndex,
  navigateToCurrentEvent,
  outputMainWindow,
  outputWindow,
  renderReport,
  scratchName,
  selectCurrentLineEventFrom,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

renderParseResult ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  Ident ->
  [ParsedOutput] ->
  m ()
renderParseResult ident output = do
  name <- displayNameByIdent ident
  when (noEventsInReport report) (throwHoist (NoEvents name))
  setL @CommandState CommandState.parseReport (Just (report, syntax))
  jumpFirst <- setting Settings.outputSelectFirst
  setL @CommandState CommandState.currentEvent (EventIndex.Absolute (first' jumpFirst))
  renderReport report syntax
  navigateToCurrentEvent =<< setting Settings.outputAutoJump
  where
    first' jumpFirst = if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)
    (report@(ParseReport events _), syntax) =
      compileReport output

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
  report <- currentReport fst
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
cycleAndNavigate offset = do
  changed <- cycleIndex offset
  when changed (navigateToCurrentEvent True)

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
