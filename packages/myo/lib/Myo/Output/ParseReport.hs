module Myo.Output.ParseReport where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.Extra (firstJust)
import Data.MonoTraversable (minimumByMay)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!?))
import Exon (exon)
import Lens.Micro.Mtl (view)
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, (</>))
import Path.IO (doesFileExist)
import Ribosome (
  Buffer,
  Handler,
  MapMode (MapNormal),
  Mapping (Mapping),
  MappingAction (MappingCall),
  MappingSpec (MappingSpec),
  Rpc,
  RpcError,
  RpcName,
  Scratch,
  ScratchId,
  ScratchState,
  Window,
  mapReport,
  resumeReport,
  scratch,
  )
import Ribosome.Api (
  bufferForFile,
  bufferGetOption,
  bufferLineCount,
  edit,
  nvimBufIsLoaded,
  nvimCwd,
  nvimWinSetBuf,
  optionList,
  redraw,
  setCursor,
  setLine,
  vimCommand,
  vimGetCurrentBuffer,
  vimGetCurrentWindow,
  vimGetWindows,
  vimSetCurrentWindow,
  windowIsValid,
  windowLine,
  windowSetOption,
  )
import qualified Ribosome.Data.FileBuffer as FileBuffer
import qualified Ribosome.Scratch as Scratch
import Ribosome.Scratch (ScratchOptions (syntax))
import Ribosome.Syntax (Syntax)

import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.OutputState (OutputState)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute (Absolute, unAbsolute))
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (
  OutputError (FileNonexistent, Internal, NoLocation, NotParsed),
  )
import Myo.Output.Data.OutputEvent (OutputEvent (OutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents (OutputEvents))
import Myo.Output.Data.ParseReport (ParseReport (ParseReport))
import Myo.Output.Data.ReportLine (ReportLine (ReportLine))

scratchId :: ScratchId
scratchId =
  "myo-report"

eventByIndex ::
  ParseReport ->
  EventIndex.Absolute ->
  Maybe OutputEventMeta
eventByIndex (ParseReport events _) (EventIndex.Absolute eventIndex) =
  events !? fromIntegral eventIndex

eventByLine ::
  ParseReport ->
  Int ->
  Maybe OutputEventMeta
eventByLine report@(ParseReport _ lines') line = do
  (ReportLine eventIndex _) <- lines' !? line
  eventByIndex report eventIndex

lineNumberByEventIndex ::
  ParseReport ->
  EventIndex.Absolute ->
  Maybe Int
lineNumberByEventIndex (ParseReport _ lines') eventIndex =
  Vector.findIndex matchEventIndex lines'
  where
    matchEventIndex (ReportLine ei _) = ei == eventIndex

findWindow ::
  Member Rpc r =>
  Window ->
  Sem r Window
findWindow outputWindow' =
  choose =<< filter (/= outputWindow') <$> vimGetWindows
  where
    choose [] = do
      vimCommand "aboveleft new"
      win <- vimGetCurrentWindow
      vimCommand "wincmd K"
      pure win
    choose (a : _) =
      pure a

filterUnloaded ::
  Member Rpc r =>
  Buffer ->
  Sem r (Maybe Buffer)
filterUnloaded buffer =
  filt <$> nvimBufIsLoaded buffer
  where
    filt True = Just buffer
    filt False = Nothing

parseAsAbsDir ::
  Path Abs Dir ->
  Text ->
  Maybe (Path Abs Dir)
parseAsAbsDir cwd path =
  parseAbsDir (toString path) <|> ((cwd </>) <$> parseRelDir (toString path))

fileInDir ::
  MonadIO m =>
  Path Rel File ->
  Path Abs Dir ->
  MaybeT m (Path Abs File)
fileInDir sub dir = do
  ifM (doesFileExist p) (pure p) (MaybeT (pure Nothing))
  where
    p =
      dir </> sub

findFileInPath ::
  Members [Rpc, Rpc !! RpcError, Embed IO] r =>
  Path Abs Dir ->
  Path Rel File ->
  Sem r (Maybe (Path Abs File))
findFileInPath cwd sub = do
  vimPath <- [] <! optionList "path"
  buf <- vimGetCurrentBuffer
  bufPath <- Text.splitOn "," <$> "" <! (bufferGetOption buf "path")
  let vimPathAbs = catMaybes (parseAsAbsDir cwd <$> (vimPath <> bufPath))
  results <- traverse (runMaybeT . fileInDir sub) vimPathAbs
  pure (firstJust id results)

findFile ::
  Members [Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Path Abs Dir ->
  Text ->
  Sem r (Path Abs File)
findFile cwd path = do
  relResult <- runMaybeT (rel cwd)
  stopNote OutputError.FileNonexistent (abs' <|> relResult)
  where
    abs' =
      parseAbsFile (toString path)
    rel cwd' = do
      relpath <- MaybeT $ pure $ parseRelFile (toString path)
      fileInDir relpath cwd' <|> MaybeT (findFileInPath cwd' relpath)

selectEventAt ::
  Member Rpc r =>
  Window ->
  Window ->
  Location ->
  Path Abs File ->
  Sem r ()
selectEventAt mainWindow outputWindow' (Location _ line col) abspath = do
  previousExists <- windowIsValid mainWindow
  window <- if previousExists && mainWindow /= outputWindow' then pure mainWindow else findWindow outputWindow'
  vimSetCurrentWindow window
  existingBuffer <- join <$> (traverse (filterUnloaded . FileBuffer.buffer) =<< bufferForFile abspath)
  maybe (edit abspath) (nvimWinSetBuf window) existingBuffer
  setCursor window line (fromMaybe 0 col)
  vimCommand "normal! zv"
  vimCommand "normal! zz"
  redraw

selectEvent ::
  Members [Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Window ->
  Window ->
  OutputEventMeta ->
  Sem r ()
selectEvent mainWindow outputWindow' (OutputEventMeta (Just loc@(Location path _ _)) _) = do
  cwd <- nvimCwd
  abspath <- findFile cwd path
  ifM (doesFileExist abspath) (selectEventAt mainWindow outputWindow' loc abspath) (stop OutputError.FileNonexistent)
selectEvent _ _ _ =
  stop OutputError.NoLocation

selectMaybeEvent ::
  Members [Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Window ->
  Window ->
  Maybe OutputEventMeta ->
  Sem r ()
selectMaybeEvent mainWindow outputWindow' =
  maybe err (selectEvent mainWindow outputWindow')
  where
    err = stop (OutputError.Internal "cursor line has no data")

selectEventByIndexFromReport ::
  Members [Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  ParseReport ->
  EventIndex.Absolute ->
  Window ->
  Window ->
  Sem r ()
selectEventByIndexFromReport report eventIndex window outputWindow' =
  selectMaybeEvent window outputWindow' (eventByIndex report eventIndex)

selectCurrentLineEventFrom ::
  Members [Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  ParseReport ->
  Window ->
  Window ->
  Sem r ()
selectCurrentLineEventFrom report outputWindow' mainWindow =
  selectMaybeEvent mainWindow outputWindow' =<< eventByLine report <$> windowLine outputWindow'

currentOutput ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r OutputState
currentOutput =
  stopNote OutputError.NotParsed =<< atomicGets CommandState.output

currentOutputCommand ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r CommandId
currentOutputCommand =
  view #command <$> currentOutput

currentEvents ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r OutputEvents
currentEvents =
  view #events <$> currentOutput

currentSyntax ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r [Syntax]
currentSyntax =
  view #syntax <$> currentOutput

currentReport ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r ParseReport
currentReport =
  stopNote OutputError.NotParsed =<< view #report <$> currentOutput

currentEvent ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Sem r EventIndex.Absolute
currentEvent =
  view #currentEvent <$> currentOutput

modifyOutput ::
  Member (AtomicState CommandState) r =>
  (OutputState -> OutputState) ->
  Sem r ()
modifyOutput f =
  atomicModify' (#output . _Just %~ f)

setOutput ::
  Member (AtomicState CommandState) r =>
  Lens' OutputState a ->
  a ->
  Sem r ()
setOutput l a =
  atomicModify' (#output . _Just . l .~ a)

cycleIndex ::
  Members [AtomicState CommandState, Stop OutputError] r =>
  Int ->
  Sem r Bool
cycleIndex offset = do
  (EventIndex.Absolute current) <- currentEvent
  report <- currentReport
  let
    eventCount =
      length (report ^. #events)
    computed =
      fromIntegral current + offset
    new =
      fromIntegral (fromMaybe 0 (computed `mod` eventCount))
  atomicModify' (#output . _Just . #currentEvent .~ EventIndex.Absolute new)
  pure (new /= current)

noScratch ::
  Member (Stop OutputError) r =>
  Maybe a ->
  Sem r a
noScratch =
  stopNote (OutputError.Internal "no scratch")

outputScratch ::
  Members [Scratch, Stop OutputError] r =>
  Sem r ScratchState
outputScratch =
  stopNote (OutputError.Internal "no scratch") =<< Scratch.find scratchId

outputMainWindow ::
  Members [Scratch, Stop OutputError] r =>
  Sem r Window
outputMainWindow =
  Scratch.previous <$> outputScratch

outputWindow ::
  Members [Scratch, Stop OutputError] r =>
  Sem r Window
outputWindow =
  Scratch.window <$> outputScratch

outputBuffer ::
  Members [Scratch, Stop OutputError] r =>
  Sem r Buffer
outputBuffer =
  Scratch.buffer <$> outputScratch

outputQuitName :: RpcName
outputQuitName =
  "MyoOutputQuit"

outputSelectName :: RpcName
outputSelectName =
  "MyoOutputSelect"

mappings :: [Mapping]
mappings =
  [
    Mapping (MappingCall outputQuitName) (MappingSpec "q" [MapNormal]) Nothing mempty,
    Mapping (MappingCall outputSelectName) (MappingSpec "<cr>" [MapNormal]) Nothing mempty
  ]

renderReport ::
  Members [Scratch, Rpc] r =>
  ParseReport ->
  [Syntax] ->
  Sem r ()
renderReport (ParseReport _ reportLines) syntax = do
  win <- view #window <$> Scratch.show (render <$> reportLines) options
  windowSetOption win "conceallevel" (3 :: Int)
  windowSetOption win "concealcursor" ("n" :: Text)
  windowSetOption win "foldmethod" ("manual" :: Text)
  where
    render (ReportLine _ text') =
      text'
    options =
      (scratch scratchId) {Scratch.mappings = mappings, syntax = syntax}

renderCurrentReport ::
  Members [AtomicState CommandState, Scratch, Rpc, Stop OutputError] r =>
  Sem r ()
renderCurrentReport = do
  report <- currentReport
  syntax <- currentSyntax
  renderReport report syntax

ensureReportScratch ::
  Members [AtomicState CommandState, Scratch, Rpc, Stop OutputError] r =>
  Sem r ()
ensureReportScratch =
  whenM (isNothing <$> Scratch.find scratchId) renderCurrentReport

navigateToEvent ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Bool ->
  EventIndex.Absolute ->
  Sem r ()
navigateToEvent jump eventIndex = do
  ensureReportScratch
  report <- currentReport
  window <- outputWindow
  mainWindow <- outputMainWindow
  buffer <- outputBuffer
  totalLines <- bufferLineCount buffer
  line <- stopNote indexErr (lineNumberByEventIndex report eventIndex)
  when (isLastEvent report) (setLine window (totalLines - 1))
  setLine window line
  when jump (selectEventByIndexFromReport report eventIndex mainWindow window)
  where
    indexErr =
      OutputError.Internal [exon|invalid event index #{show eventIndex}|]
    isLastEvent report =
      fromIntegral (EventIndex.unAbsolute eventIndex) == (length (report ^. #events) - 1)

navigateToCurrentEvent ::
  Members [AtomicState CommandState, Scratch, Rpc, Rpc !! RpcError, Stop OutputError, Embed IO] r =>
  Bool ->
  Sem r ()
navigateToCurrentEvent jump =
  navigateToEvent jump =<< currentEvent

levelLens :: Lens' OutputEvent Int
levelLens =
  #meta . #level

compareEventFilterLevel :: OutputEvent -> OutputEvent -> Ordering
compareEventFilterLevel e1 e2 =
  compare (view levelLens e1) (view levelLens e2)

filterEventLevel :: Int -> Vector OutputEvent -> Vector OutputEvent
filterEventLevel maxLevel events =
  Vector.filter levelHigher events
  where
    levelHigher event =
      view levelLens event <= effectiveMaxLevel
    effectiveMaxLevel =
      max maxLevel (fromMaybe 0 lowestEventLevel)
    lowestEventLevel =
      view levelLens <$> minimumByMay compareEventFilterLevel events

compileReport :: Int -> OutputEvents -> ParseReport
compileReport maxLevel (OutputEvents events) =
  process events
  where
    process =
      uncurry ParseReport . second join . Vector.unzip . Vector.imap reindexEvent . filterEventLevel maxLevel
    reindexEvent index (OutputEvent meta lines') =
      (meta, absoluteDir index <$> lines')
    absoluteDir index (ReportLine _ text') =
      ReportLine (EventIndex.Absolute (fromIntegral index)) text'

myoOutputQuit ::
  Member (Scratch !! RpcError) r =>
  Handler r ()
myoOutputQuit =
  resumeReport (Scratch.kill scratchId)

myoOutputSelect ::
  Members [AtomicState CommandState, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Handler r ()
myoOutputSelect =
  resumeReport @Rpc $ resumeReport @Scratch $ mapReport do
    mainWindow <- outputMainWindow
    window <- outputWindow
    report <- currentReport
    selectCurrentLineEventFrom report window mainWindow
