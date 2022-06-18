module Myo.Command.Output where

-- import GHC.Natural (minusNaturalMaybe)
-- import Ribosome.Data.SettingError (SettingError)
-- import Ribosome.Scratch (killScratchByName)

-- import Myo.Command.Data.CommandState (CommandState)
-- import qualified Myo.Command.Data.CommandState as OutputState (currentEvent, report)
-- import Myo.Command.History (displayNameByIdent)
-- import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute (Absolute))
-- import Myo.Output.Data.OutputError (OutputError (NoEvents))
-- import Myo.Output.Data.ParseReport (ParseReport (ParseReport), noEventsInReport)
-- import Myo.Output.ParseReport (
--   compileReport,
--   currentEvents,
--   currentOutputCommand,
--   currentReport,
--   currentSyntax,
--   cycleIndex,
--   navigateToCurrentEvent,
--   outputMainWindow,
--   outputWindow,
--   renderReport,
--   scratchName,
--   selectCurrentLineEventFrom,
--   setOutput,
--   )
-- import qualified Myo.Settings as Settings (outputAutoJump, outputSelectFirst)

-- initialRenderReport ::
--   Member (AtomicState Env) r =>
--   ParseReport ->
--   m ()
-- initialRenderReport report@(ParseReport events _) = do
--   jumpFirst <- setting Settings.outputSelectFirst
--   setOutput OutputState.currentEvent (EventIndex.Absolute (first' jumpFirst))
--   syntax <- currentSyntax
--   renderReport report syntax
--   navigateToCurrentEvent =<< setting Settings.outputAutoJump
--   where
--     first' jumpFirst =
--       if jumpFirst then 0 else fromMaybe 0 (minusNaturalMaybe (fromIntegral (length events)) 1)

-- compileAndRenderReport ::
--   Member (AtomicState Env) r =>
--   m ()
-- compileAndRenderReport = do
--   report <- compileReport 0 <$> currentEvents
--   ident <- currentOutputCommand
--   when (noEventsInReport report) (stop . NoEvents =<< displayNameByIdent ident)
--   setOutput OutputState.report (Just report)
--   initialRenderReport report

-- outputQuit ::
--   m ()
-- outputQuit =
--   killScratchByName scratchName

-- outputSelect ::
--   Member (AtomicState Env) r =>
--   m ()
-- outputSelect = do
--   mainWindow <- outputMainWindow
--   window <- outputWindow
--   report <- currentReport
--   selectCurrentLineEventFrom report window mainWindow

-- cycleAndNavigate ::
--   Member (AtomicState Env) r =>
--   Int ->
--   m ()
-- cycleAndNavigate offset =
--   whenM (cycleIndex offset) (navigateToCurrentEvent True)

-- myoPrev ::
--   Member (AtomicState Env) r =>
--   m ()
-- myoPrev =
--   cycleAndNavigate (-1)

-- myoNext ::
--   Member (AtomicState Env) r =>
--   m ()
-- myoNext =
--   cycleAndNavigate 1
