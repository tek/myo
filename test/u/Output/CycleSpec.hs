{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.CycleSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Api.Window (currentCursor, cursor)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Plugin.Mapping (executeMapping)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Test.Ui (windowCountIs)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))
import Test.Framework

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseResult)
import Myo.Command.Output (myoNext, renderParseResult)
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
import Myo.Plugin (mappingOutputSelect)
import qualified Myo.Settings as Settings (outputJumpFirst)

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

outputNextSpec :: MyoN ()
outputNextSpec = do
  file <- fixture $ "output" </> "select" </> "File.hs"
  let po = [parsedOutput file]
  updateSetting Settings.outputJumpFirst True
  initialize''
  setL @CommandState CommandState.parseResult (Just (ParseResult (Ident.Str "test") po))
  renderParseResult (Ident.Str "test") po
  windowCountIs 2
  ow <- outputWindow
  myoNext
  (line, col) <- currentCursor
  gassertEqual (3, 4) (line, col)
  (oline, ocol) <- cursor ow
  gassertEqual (5, 0) (oline, ocol)

test_outputNext :: IO ()
test_outputNext =
  tmuxGuiSpecDef outputNextSpec
