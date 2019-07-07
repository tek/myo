{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.CycleSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Test.Tmux (tmuxSpecDef)
import Ribosome.Test.Ui (currentCursorIs, cursorIs, windowCountIs)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))
import Test.Framework

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseResult)
import Myo.Command.Output (myoNext, myoPrev, renderParseResult)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParseResult (ParseResult(ParseResult))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Output.ParseReport (outputWindow)
import qualified Myo.Settings as Settings (outputSelectFirst)

loc1 :: Text -> Maybe Location
loc1 file =
  Just $ Location file 9 (Just 2)

loc2 :: Text -> Maybe Location
loc2 file =
  Just $ Location file 3 (Just 4)

events :: Text -> Vector OutputEvent
events file =
  Vector.fromList [OutputEvent (loc1 file) 0, OutputEvent (loc2 file) 1]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: Text -> ParsedOutput
parsedOutput file =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent (events file) messages))

cycleSpecRender :: Myo Window
cycleSpecRender = do
  file <- fixture $ "output" </> "select" </> "File.hs"
  let po = [parsedOutput (toText file)]
  initialize''
  setL @CommandState CommandState.parseResult (Just (ParseResult (Ident.Str "test") po))
  renderParseResult (Ident.Str "test") po
  windowCountIs 2
  outputWindow

outputPrevSpec :: Myo ()
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
  tmuxSpecDef outputPrevSpec

outputNextSpec :: Myo ()
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
  tmuxSpecDef outputNextSpec
