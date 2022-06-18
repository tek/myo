module Myo.Output.ParseReport where

-- import Control.Lens (Lens', _Just, ifolded, over, set, view, views, withIndex)
-- import Data.MonoTraversable (minimumByMay)
-- import qualified Data.Text as Text
-- import Data.Vector (Vector, (!?))
-- import qualified Data.Vector as Vector (filter, findIndex, unzip)
-- import Data.Vector.Lens (toVectorOf)
-- import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile, toFilePath, (</>))
-- import Path.IO (doesFileExist)
-- import Ribosome.Api.Buffer (bufferForFile, edit)
-- import Ribosome.Api.Option (optionList)
-- import Ribosome.Api.Path (nvimCwd)
-- import Ribosome.Api.Window (redraw, setCursor, setLine, windowLine)
-- import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
-- import qualified Ribosome.Data.Scratch as Scratch (scratchWindow)
-- import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchMappings, scratchSyntax)
-- import Ribosome.Data.Syntax (Syntax)
-- import Ribosome.Msgpack.Error (DecodeError)
-- import Ribosome.Nvim.Api.Data (Buffer, Window)
-- import Ribosome.Nvim.Api.IO (
--   bufferGetOption,
--   bufferLineCount,
--   nvimBufIsLoaded,
--   nvimWinSetBuf,
--   vimCommand,
--   vimGetCurrentBuffer,
--   vimGetCurrentWindow,
--   vimGetWindows,
--   vimSetCurrentWindow,
--   windowIsValid,
--   windowSetOption,
--   )
-- import Ribosome.Scratch (lookupScratch, scratchBuffer, scratchPreviousWindow, scratchWindow, showInScratch)

-- import Myo.Command.Data.CommandState (CommandState, OutputState)
-- import qualified Myo.Command.Data.CommandState as CommandState (output)
-- import qualified Myo.Command.Data.CommandState as OutputState (command, currentEvent, events, report, syntax)
-- import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute(Absolute, unAbsolute))
-- import Myo.Output.Data.Location (Location(Location))
-- import Myo.Output.Data.OutputError (OutputError)
-- import qualified Myo.Output.Data.OutputError as OutputError (
--   OutputError(Internal, NoLocation, NotParsed, FileNonexistent),
--   )
-- import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent), OutputEventMeta(OutputEventMeta))
-- import qualified Myo.Output.Data.OutputEvent as OutputEvent (meta)
-- import qualified Myo.Output.Data.OutputEvent as OutputEventMeta (level)
-- import Myo.Output.Data.OutputEvents (OutputEvents(OutputEvents))
-- import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
-- import qualified Myo.Output.Data.ParseReport as ParseReport (events)
-- import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

-- scratchName :: Text
-- scratchName =
--   "myo-report"

-- eventByIndex ::
--   ParseReport ->
--   EventIndex.Absolute ->
--   Maybe OutputEventMeta
-- eventByIndex (ParseReport events _) (EventIndex.Absolute eventIndex) =
--   events !? fromIntegral eventIndex

-- eventByLine ::
--   ParseReport ->
--   Int ->
--   Maybe OutputEventMeta
-- eventByLine report@(ParseReport _ lines') line = do
--   (ReportLine eventIndex _) <- lines' !? line
--   eventByIndex report eventIndex

-- lineNumberByEventIndex ::
--   ParseReport ->
--   EventIndex.Absolute ->
--   Maybe Int
-- lineNumberByEventIndex (ParseReport _ lines') eventIndex =
--   Vector.findIndex matchEventIndex lines'
--   where
--     matchEventIndex (ReportLine ei _) = ei == eventIndex

-- findWindow ::
--   Window ->
--   m Window
-- findWindow outputWindow' =
--   choose =<< filter (/= outputWindow') <$> vimGetWindows
--   where
--     choose [] = do
--       vimCommand "aboveleft new"
--       win <- vimGetCurrentWindow
--       vimCommand "wincmd K"
--       pure win
--     choose (a : _) =
--       pure a

-- filterUnloaded ::
--   Buffer ->
--   m (Maybe Buffer)
-- filterUnloaded buffer =
--   filt <$> nvimBufIsLoaded buffer
--   where
--     filt True = Just buffer
--     filt False = Nothing

-- parseAsAbsDir ::
--   Path Abs Dir ->
--   Text ->
--   Maybe (Path Abs Dir)
-- parseAsAbsDir cwd path =
--   parseAbsDir (toString path) <|> ((cwd </>) <$> parseRelDir (toString path))

-- fileInDir ::
--   MonadIO m =>
--   Path Rel File ->
--   Path Abs Dir ->
--   MaybeT m (Path Abs File)
-- fileInDir sub dir = do
--   ifM (doesFileExist p) (pure p) (MaybeT (pure Nothing))
--   where
--     p = dir </> sub

-- findFileInPath ::
--   MonadIO m =>
--   Path Abs Dir ->
--   Path Rel File ->
--   MaybeT m (Path Abs File)
-- findFileInPath cwd sub = do
--   vimPath <- lift $ catchAs @RpcError [] (optionList "path")
--   buf <- vimGetCurrentBuffer
--   bufPath <- Text.splitOn "," <$> catchAs @RpcError "" (bufferGetOption buf "path")
--   let vimPathAbs = catMaybes (parseAsAbsDir cwd <$> (vimPath <> bufPath))
--   results <- lift $ traverse (runMaybeT . fileInDir sub) vimPathAbs
--   head <$> (MaybeT . pure . nonEmpty . catMaybes $ results)

-- findFile ::
--   ∀ e m .
--   MonadIO m =>
--   Path Abs Dir ->
--   Text ->
--   m (Path Abs File)
-- findFile cwd path = do
--   relResult <- runMaybeT (rel cwd)
--   stopNote OutputError.FileNonexistent (abs' <|> relResult)
--   where
--     abs' =
--       parseAbsFile (toString path)
--     rel cwd' = do
--       relpath <- MaybeT $ pure $ parseRelFile (toString path)
--       fileInDir relpath cwd' <|> findFileInPath cwd' relpath

-- selectEventAt ::
--   Window ->
--   Window ->
--   Location ->
--   FilePath ->
--   m ()
-- selectEventAt mainWindow outputWindow' (Location _ line col) abspath = do
--   previousExists <- windowIsValid mainWindow
--   window <- if previousExists && mainWindow /= outputWindow' then pure mainWindow else findWindow outputWindow'
--   vimSetCurrentWindow window
--   existingBuffer <- join <$> (traverse filterUnloaded =<< bufferForFile (toText abspath))
--   maybe (edit abspath) (nvimWinSetBuf window) existingBuffer
--   setCursor window line (fromMaybe 0 col)
--   vimCommand "normal! zv"
--   vimCommand "normal! zz"
--   redraw

-- locationExists ::
--   MonadIO m =>
--   FilePath ->
--   m Bool
-- locationExists path =
--   fromMaybe False <$> traverse doesFileExist (parseAbsFile path)

-- selectEvent ::
--   Window ->
--   Window ->
--   OutputEventMeta ->
--   m ()
-- selectEvent mainWindow outputWindow' (OutputEventMeta (Just loc@(Location path _ _)) _) = do
--   cwd <- stopNote (OutputError.Internal "bad cwd") . parseAbsDir =<< nvimCwd
--   abspath <- toFilePath <$> findFile cwd path
--   ifM (locationExists abspath) (selectEventAt mainWindow outputWindow' loc abspath) (stop OutputError.FileNonexistent)
-- selectEvent _ _ _ =
--   stop OutputError.NoLocation

-- selectMaybeEvent ::
--   Window ->
--   Window ->
--   Maybe OutputEventMeta ->
--   m ()
-- selectMaybeEvent mainWindow outputWindow' =
--   maybe err (selectEvent mainWindow outputWindow')
--   where
--     err = stop (OutputError.Internal "cursor line has no data")

-- selectEventByIndexFromReport ::
--   ParseReport ->
--   EventIndex.Absolute ->
--   Window ->
--   Window ->
--   m ()
-- selectEventByIndexFromReport report eventIndex window outputWindow' =
--   selectMaybeEvent window outputWindow' $ eventByIndex report eventIndex

-- selectCurrentLineEventFrom ::
--   ParseReport ->
--   Window ->
--   Window ->
--   m ()
-- selectCurrentLineEventFrom report outputWindow' mainWindow =
--   selectMaybeEvent mainWindow outputWindow' =<< eventByLine report <$> windowLine outputWindow'

-- currentOutput ::
--   Member (AtomicState Env) r =>
--   m OutputState
-- currentOutput =
--   stopNote OutputError.NotParsed =<< atomicGets CommandState.output

-- currentOutputCommand ::
--   Member (AtomicState Env) r =>
--   m Ident
-- currentOutputCommand =
--   view OutputState.command <$> currentOutput

-- currentEvents ::
--   Member (AtomicState Env) r =>
--   m OutputEvents
-- currentEvents =
--   view OutputState.events <$> currentOutput

-- currentSyntax ::
--   Member (AtomicState Env) r =>
--   m [Syntax]
-- currentSyntax =
--   view OutputState.syntax <$> currentOutput

-- currentReport ::
--   Member (AtomicState Env) r =>
--   m ParseReport
-- currentReport =
--   stopNote OutputError.NotParsed =<< view OutputState.report <$> currentOutput

-- currentEvent ::
--   Member (AtomicState Env) r =>
--   m EventIndex.Absolute
-- currentEvent =
--   view OutputState.currentEvent <$> currentOutput

-- modifyOutput ::
--   Member (AtomicState Env) r =>
--   (OutputState -> OutputState) ->
--   m ()
-- modifyOutput f =
--   modifyL @CommandState CommandState.output (over _Just f)

-- setOutput ::
--   Member (AtomicState Env) r =>
--   Lens' OutputState a ->
--   a ->
--   m ()
-- setOutput lens a =
--   modifyL @CommandState CommandState.output (set (_Just . lens) a)

-- cycleIndex ::
--   Member (AtomicState Env) r =>
--   Int ->
--   m Bool
-- cycleIndex offset = do
--   (EventIndex.Absolute current) <- currentEvent
--   total <- views ParseReport.events length <$> currentReport
--   let new = fromIntegral $ (fromIntegral current + offset) `mod` total
--   modifyL @CommandState CommandState.output (set (_Just . OutputState.currentEvent) (EventIndex.Absolute new))
--   pure (new /= current)

-- scratchErrorMaybe ::
--   Maybe a ->
--   m a
-- scratchErrorMaybe =
--   stopNote (OutputError.Internal "no scratch")

-- outputMainWindow ::
--   m Window
-- outputMainWindow =
--   scratchErrorMaybe =<< scratchPreviousWindow scratchName

-- outputWindow ::
--   m Window
-- outputWindow =
--   scratchErrorMaybe =<< scratchWindow scratchName

-- outputBuffer ::
--   m Buffer
-- outputBuffer =
--   scratchErrorMaybe =<< scratchBuffer scratchName

-- mappings :: [Mapping]
-- mappings =
--   [
--     Mapping (MappingIdent "output-quit") "q" "n" False True,
--     Mapping (MappingIdent "output-select") "<cr>" "n" False True
--     ]

-- renderReport ::
--   ParseReport ->
--   [Syntax] ->
--   m ()
-- renderReport (ParseReport _ lines') syntax = do
--   win <- Scratch.scratchWindow <$> showInScratch (render <$> lines') options
--   windowSetOption win "conceallevel" (toMsgpack (3 :: Int))
--   windowSetOption win "concealcursor" (toMsgpack ("n" :: Text))
--   windowSetOption win "foldmethod" (toMsgpack ("manual" :: Text))
--   where
--     render (ReportLine _ text') =
--       text'
--     options =
--       scratchMappings mappings . scratchSyntax syntax . defaultScratchOptions $ scratchName

-- renderCurrentReport ::
--   Member (AtomicState Env) r =>
--   m ()
-- renderCurrentReport = do
--   report <- currentReport
--   syntax <- currentSyntax
--   renderReport report syntax

-- ensureReportScratch ::
--   Member (AtomicState Env) r =>
--   m ()
-- ensureReportScratch =
--   whenM (isNothing <$> lookupScratch scratchName) renderCurrentReport

-- navigateToEvent ::
--   Member (AtomicState Env) r =>
--   Bool ->
--   EventIndex.Absolute ->
--   m ()
-- navigateToEvent jump eventIndex = do
--   ensureReportScratch
--   report <- currentReport
--   window <- outputWindow
--   mainWindow <- outputMainWindow
--   buffer <- outputBuffer
--   totalLines <- bufferLineCount buffer
--   line <- stopNote indexErr $ lineNumberByEventIndex report eventIndex
--   when (isLastEvent report) (setLine window (totalLines - 1))
--   setLine window line
--   when jump (selectEventByIndexFromReport report eventIndex mainWindow window)
--   where
--     indexErr =
--       OutputError.Internal $ "invalid event index " <> show eventIndex
--     isLastEvent report =
--       fromIntegral (EventIndex.unAbsolute eventIndex) == eventCount report - 1
--     eventCount =
--       views ParseReport.events length

-- navigateToCurrentEvent ::
--   Member (AtomicState Env) r =>
--   Bool ->
--   m ()
-- navigateToCurrentEvent jump =
--   navigateToEvent jump =<< currentEvent

-- levelLens :: Lens' OutputEvent Int
-- levelLens =
--   OutputEvent.meta . OutputEventMeta.level

-- compareEventFilterLevel :: OutputEvent -> OutputEvent -> Ordering
-- compareEventFilterLevel e1 e2 =
--   compare (view levelLens e1) (view levelLens e2)

-- filterEventLevel :: Int -> Vector OutputEvent -> Vector OutputEvent
-- filterEventLevel maxLevel events =
--   Vector.filter levelHigher events
--   where
--     levelHigher event =
--       view levelLens event <= effectiveMaxLevel
--     effectiveMaxLevel =
--       max maxLevel (fromMaybe 0 lowestEventLevel)
--     lowestEventLevel =
--       view levelLens <$> minimumByMay compareEventFilterLevel events

-- compileReport :: Int -> OutputEvents -> ParseReport
-- compileReport maxLevel (OutputEvents events) =
--   process events
--   where
--     process =
--       uncurry ParseReport . second join . Vector.unzip . fmap reindexEvent . zipWithIndex . filterEventLevel maxLevel
--     reindexEvent (index, OutputEvent meta lines') =
--       (meta, absoluteDir index <$> lines')
--     absoluteDir index (ReportLine _ text') =
--       ReportLine (EventIndex.Absolute (fromIntegral index)) text'
--     zipWithIndex =
--       toVectorOf (ifolded . withIndex)
