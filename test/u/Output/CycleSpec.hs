{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.CycleSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Test.Ui (currentCursorIs, cursorIs, windowCountIs)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))
import Test.Framework

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseResult)
import Myo.Command.Output (myoNext, myoPrev, renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParseResult (ParseResult(ParseResult))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine)
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.ParseReport (outputWindow)
import qualified Myo.Settings as Settings (outputSelectFirst)

loc1 :: FilePath -> Location
loc1 file =
  Location file 9 (Just 2)

loc2 :: FilePath -> Location
loc2 file =
  Location file 3 (Just 4)

events :: FilePath -> Vector OutputEvent
events file =
  Vector.fromList [OutputEvent (Just (loc1 file)) 0, OutputEvent (Just (loc2 file)) 1]

reportLines :: FilePath -> Vector ReportLine
reportLines file =
  formatReportLine 0 (loc1 file) (FoundReq1 "TypeA" "TypeB") <> formatReportLine 1 (loc2 file) (NoMethod "fmap")

parsedOutput :: FilePath -> ParsedOutput
parsedOutput file =
  ParsedOutput haskellSyntax (const $ ParseReport (events file) (reportLines file))

cycleSpecRender :: MyoN Window
cycleSpecRender = do
  file <- fixture $ "output" </> "select" </> "File.hs"
  let po = [parsedOutput file]
  initialize''
  setL @CommandState CommandState.parseResult (Just (ParseResult (Ident.Str "test") po))
  renderParseResult (Ident.Str "test") po
  windowCountIs 2
  outputWindow

outputPrevSpec :: MyoN ()
outputPrevSpec = do
  updateSetting Settings.outputSelectFirst False
  ow <- cycleSpecRender
  currentCursorIs 3 4
  cursorIs 5 0 ow
  myoPrev
  currentCursorIs 9 2
  cursorIs 0 0 ow

test_outputPrev :: IO ()
test_outputPrev =
  tmuxGuiSpecDef outputPrevSpec

outputNextSpec :: MyoN ()
outputNextSpec = do
  updateSetting Settings.outputSelectFirst True
  ow <- cycleSpecRender
  currentCursorIs 9 2
  cursorIs 0 0 ow
  myoNext
  currentCursorIs 3 4
  cursorIs 5 0 ow

test_outputNext :: IO ()
test_outputNext =
  tmuxGuiSpecDef outputNextSpec
