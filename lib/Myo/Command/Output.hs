module Myo.Command.Output where

import Chiasma.Data.Ident (Ident)
import Control.Monad (when)
import Control.Monad.DeepState (MonadDeepState, setL)
import Control.Monad.IO.Class (MonadIO)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import Ribosome.Data.Scratch (scratchWindow)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchMappings, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (windowSetOption)
import Ribosome.Scratch (killScratchByName, showInScratch)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (currentEvent, parseReport)
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Myo.Output.Data.OutputEvent (EventIndex(EventIndex))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport), noEventsInReport)
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.ParseReport (
  compileReport,
  currentReport,
  cycleIndex,
  navigateToCurrentEvent,
  outputMainWindow,
  outputWindow,
  scratchName,
  selectCurrentLineEventFrom,
  )
import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

mappings :: [Mapping]
mappings =
  [
    Mapping (MappingIdent "output-quit") "q" "n" False True,
    Mapping (MappingIdent "output-select") "<cr>" "n" False True
    ]

renderReport ::
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  ParseReport ->
  [Syntax] ->
  m ()
renderReport (ParseReport _ lines') syntax = do
  win <- scratchWindow <$> showInScratch (render <$> lines') options
  windowSetOption win "conceallevel" (toMsgpack (3 :: Int))
  windowSetOption win "concealcursor" (toMsgpack ("n" :: Text))
  windowSetOption win "foldmethod" (toMsgpack ("manual" :: Text))
  where
    render (ReportLine _ text) =
      text
    options =
      scratchMappings mappings . scratchSyntax syntax . defaultScratchOptions $ scratchName

renderParseResult ::
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  Ident ->
  [ParsedOutput] ->
  m ()
renderParseResult ident output = do
  when (noEventsInReport report) (throwHoist (NoEvents ident))
  setL @CommandState CommandState.parseReport (Just report)
  jumpFirst <- setting Settings.outputSelectFirst
  setL @CommandState CommandState.currentEvent (EventIndex (first' jumpFirst))
  renderReport report syntax
  navigateToCurrentEvent =<< setting Settings.outputAutoJump
  where
    first' jumpFirst = if jumpFirst then 0 else length events - 1
    (report@(ParseReport events _), syntax) =
      compileReport output

outputQuit ::
  MonadRibo m =>
  NvimE e m =>
  m ()
outputQuit =
  killScratchByName scratchName

outputSelect ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
outputSelect = do
  mainWindow <- outputMainWindow
  window <- outputWindow
  -- (ParseResult ident _) <- hoistMaybe NotParsed =<< getL @CommandState CommandState.parseResult
  report <- currentReport id
  selectCurrentLineEventFrom report window mainWindow

cycleAndNavigate ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  Int ->
  m ()
cycleAndNavigate offset = do
  changed <- cycleIndex offset
  when changed (navigateToCurrentEvent True)

myoPrev ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
myoPrev =
  cycleAndNavigate (-1)

myoNext ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
myoNext =
  cycleAndNavigate 1
