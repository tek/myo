module Myo.Output.ParseReport where

import qualified Control.Lens as Lens (views)
import Control.Monad.DeepError (hoistMaybe)
import Data.Vector ((!?))
import qualified Data.Vector as Vector (findIndex)
import Ribosome.Api.Buffer (bufferForFile, edit)
import Ribosome.Api.Window (redraw, setCursor, setLine, windowLine)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Nvim.Api.IO (nvimWinSetBuf, vimCommand, vimSetCurrentWindow, windowIsValid)
import Ribosome.Scratch (scratchPreviousWindow, scratchWindow)

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

findWindow :: m Window
findWindow =
  undefined

selectEvent ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  OutputEvent ->
  m ()
selectEvent mainWindow (OutputEvent (Just (Location path line col)) _) = do
  previousExists <- windowIsValid mainWindow
  window <- if previousExists then return mainWindow else findWindow
  vimSetCurrentWindow window
  existingBuffer <- bufferForFile (toText path)
  maybe (edit path) (nvimWinSetBuf window) existingBuffer
  setCursor window line (fromMaybe 0 col)
  vimCommand "normal! zv"
  redraw
selectEvent _ _ =
  throwHoist OutputError.NoLocation

selectMaybeEvent ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
  Window ->
  Maybe OutputEvent ->
  m ()
selectMaybeEvent mainWindow =
  maybe err (selectEvent mainWindow)
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
  m ()
selectEventByIndexFromReport report eventIndex window =
  selectMaybeEvent window $ eventByIndex report eventIndex

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
  selectMaybeEvent mainWindow =<< eventByLine report <$> windowLine outputWindow'

currentReport ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  (ParseReport -> a) ->
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
  total <- currentReport (Lens.views ParseReport.events length)
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

navigateToEvent ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  Bool ->
  EventIndex ->
  m ()
navigateToEvent jump eventIndex = do
  report <- currentReport id
  window <- outputWindow
  mainWindow <- outputMainWindow
  line <- hoistMaybe indexErr $ lineNumberByEventIndex report eventIndex
  setLine window line
  when jump (selectEventByIndexFromReport report eventIndex mainWindow)
  where
    indexErr = OutputError.Internal $ "invalid event index " <> show eventIndex

navigateToCurrentEvent ::
  MonadDeepState s CommandState m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadIO m =>
  MonadRibo m =>
  NvimE e m =>
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
