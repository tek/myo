module Myo.Output.ParseReport where

import qualified Control.Lens as Lens (_1, views)
import Control.Monad.DeepError (hoistMaybe)
import Data.Vector ((!?))
import qualified Data.Vector as Vector (findIndex)
import Ribosome.Api.Buffer (bufferForFile, edit)
import Ribosome.Api.Window (redraw, setCursor, setLine, windowLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import qualified Ribosome.Data.Scratch as Scratch (scratchWindow)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchMappings, scratchSyntax)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Buffer, Window)
import Ribosome.Nvim.Api.IO (
  nvimBufIsLoaded,
  nvimWinSetBuf,
  vimCommand,
  vimGetCurrentWindow,
  vimGetWindows,
  vimSetCurrentWindow,
  windowIsValid,
  windowSetOption,
  )
import Ribosome.Scratch (lookupScratch, scratchPreviousWindow, scratchWindow, showInScratch)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (currentEvent, parseReport)
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Internal, NoLocation, NotParsed))
import Myo.Output.Data.OutputEvent (EventIndex(EventIndex), OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import qualified Myo.Output.Data.ParseReport as ParseReport (events)
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (ParsedOutput(_syntax))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

scratchName :: Text
scratchName =
  "myo-report"

eventByIndex ::
  ParseReport ->
  EventIndex ->
  Maybe OutputEvent
eventByIndex (ParseReport events _) (EventIndex eventIndex) =
  events !? eventIndex

eventByLine ::
  ParseReport ->
  Int ->
  Maybe OutputEvent
eventByLine report@(ParseReport _ lines') line = do
  (ReportLine eventIndex _) <- lines' !? line
  eventByIndex report eventIndex

lineNumberByEventIndex ::
  ParseReport ->
  EventIndex ->
  Maybe Int
lineNumberByEventIndex (ParseReport _ lines') eventIndex =
  Vector.findIndex matchEventIndex lines'
  where
    matchEventIndex (ReportLine ei _) = ei == eventIndex

findWindow ::
  NvimE e m =>
  Window ->
  m Window
findWindow outputWindow' =
  choose =<< filter (/= outputWindow') <$> vimGetWindows
  where
    choose [] = do
      vimCommand "aboveleft new"
      win <- vimGetCurrentWindow
      vimCommand "wincmd K"
      return win
    choose (a : _) =
      return a

filterUnloaded ::
  NvimE e m =>
  Buffer ->
  m (Maybe Buffer)
filterUnloaded buffer =
  filt <$> nvimBufIsLoaded buffer
  where
    filt True = Just buffer
    filt False = Nothing

selectEvent ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  Window ->
  OutputEvent ->
  m ()
selectEvent mainWindow outputWindow' (OutputEvent (Just (Location path line col)) _) = do
  previousExists <- windowIsValid mainWindow
  window <- if previousExists && mainWindow /= outputWindow' then return mainWindow else findWindow outputWindow'
  vimSetCurrentWindow window
  existingBuffer <- join <$> (traverse filterUnloaded =<< bufferForFile (toText path))
  maybe (edit (toString path)) (nvimWinSetBuf window) existingBuffer
  setCursor window line (fromMaybe 0 col)
  vimCommand "normal! zv"
  vimCommand "normal! zz"
  redraw
selectEvent _ _ _ =
  throwHoist OutputError.NoLocation

selectMaybeEvent ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  Window ->
  Maybe OutputEvent ->
  m ()
selectMaybeEvent mainWindow outputWindow' =
  maybe err (selectEvent mainWindow outputWindow')
  where
    err = throwHoist (OutputError.Internal "cursor line has no data")

selectEventByIndexFromReport ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  ParseReport ->
  EventIndex ->
  Window ->
  Window ->
  m ()
selectEventByIndexFromReport report eventIndex window outputWindow' =
  selectMaybeEvent window outputWindow' $ eventByIndex report eventIndex

selectCurrentLineEventFrom ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  ParseReport ->
  Window ->
  Window ->
  m ()
selectCurrentLineEventFrom report outputWindow' mainWindow =
  selectMaybeEvent mainWindow outputWindow' =<< eventByLine report <$> windowLine outputWindow'

currentReport ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  ((ParseReport, [Syntax]) -> a) ->
  m a
currentReport f =
  f <$> (hoistMaybe OutputError.NotParsed =<< getL @CommandState CommandState.parseReport)

currentEvent ::
  MonadDeepState s CommandState m =>
  m EventIndex
currentEvent =
  getL @CommandState CommandState.currentEvent

cycleIndex ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  Int ->
  m Bool
cycleIndex offset = do
  (EventIndex current) <- currentEvent
  total <- currentReport (Lens.views (Lens._1 . ParseReport.events) length)
  let new = (current + offset) `mod` total
  setL @CommandState CommandState.currentEvent (EventIndex new)
  return (new /= current)

scratchErrorMaybe ::
  MonadDeepError e OutputError m =>
  Maybe a ->
  m a
scratchErrorMaybe =
  hoistMaybe (OutputError.Internal "no scratch")

outputMainWindow ::
  MonadDeepError e OutputError m =>
  MonadRibo m =>
  m Window
outputMainWindow =
  scratchErrorMaybe =<< scratchPreviousWindow scratchName

outputWindow ::
  MonadDeepError e OutputError m =>
  MonadRibo m =>
  m Window
outputWindow =
  scratchErrorMaybe =<< scratchWindow scratchName

mappings :: [Mapping]
mappings =
  [
    Mapping (MappingIdent "output-quit") "q" "n" False True,
    Mapping (MappingIdent "output-select") "<cr>" "n" False True
    ]

renderReport ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  ParseReport ->
  [Syntax] ->
  m ()
renderReport (ParseReport _ lines') syntax = do
  win <- Scratch.scratchWindow <$> showInScratch (render <$> lines') options
  windowSetOption win "conceallevel" (toMsgpack (3 :: Int))
  windowSetOption win "concealcursor" (toMsgpack ("n" :: Text))
  windowSetOption win "foldmethod" (toMsgpack ("manual" :: Text))
  where
    render (ReportLine _ text) =
      text
    options =
      scratchMappings mappings . scratchSyntax syntax . defaultScratchOptions $ scratchName

renderCurrentReport ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e OutputError m =>
  m ()
renderCurrentReport =
  uncurry renderReport =<< currentReport id

ensureReportScratch ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e OutputError m =>
  m ()
ensureReportScratch =
  whenM (isNothing <$> lookupScratch scratchName) renderCurrentReport

navigateToEvent ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Bool ->
  EventIndex ->
  m ()
navigateToEvent jump eventIndex = do
  ensureReportScratch
  report <- currentReport fst
  window <- outputWindow
  mainWindow <- outputMainWindow
  line <- hoistMaybe indexErr $ lineNumberByEventIndex report eventIndex
  setLine window line
  when jump (selectEventByIndexFromReport report eventIndex mainWindow window)
  where
    indexErr = OutputError.Internal $ "invalid event index " <> show eventIndex

navigateToCurrentEvent ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  Bool ->
  m ()
navigateToCurrentEvent jump =
  navigateToEvent jump =<< currentEvent

compileReport :: [ParsedOutput] -> (ParseReport, [Syntax])
compileReport output =
  (mconcat reports, syntax)
  where
    syntax = ParsedOutput._syntax <$> output
    (_, reports) = mapAccumL format 0 output
    format offset (ParsedOutput _ cons) =
      (offset + length events, report')
      where
        report'@(ParseReport events _) = cons offset
