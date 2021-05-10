module Myo.Test.Output.CycleTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.Data (Window)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Tmux (tmuxTestDef)
import Ribosome.Test.Ui (currentCursorIs, cursorIs, windowCountIs)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))

import Myo.Command.Output (compileAndRenderReport, myoNext, myoPrev)
import Myo.Command.Parse (storeParseResult)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Output.ParseReport (outputWindow)
import qualified Myo.Settings as Settings (outputSelectFirst)
import Myo.Test.Unit (MyoTest)

loc1 :: Text -> Maybe Location
loc1 file =
  Just $ Location file 9 (Just 2)

loc2 :: Text -> Maybe Location
loc2 file =
  Just $ Location file 3 (Just 4)

events :: Text -> Vector OutputEventMeta
events file =
  Vector.fromList [OutputEventMeta (loc1 file) 0, OutputEventMeta (loc2 file) 0]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: Text -> ParsedOutput
parsedOutput file =
  ParsedOutput def (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent (events file) messages))

cycleTestRender :: MyoTest Window
cycleTestRender = do
  file <- fixture $ "output" </> "select" </> "File.hs"
  let po = [parsedOutput (toText file)]
  lift initialize''
  storeParseResult (Ident.Str "test") po
  compileAndRenderReport
  windowCountIs 2
  outputWindow

outputPrevTest :: MyoTest ()
outputPrevTest = do
  updateSetting Settings.outputSelectFirst False
  ow <- cycleTestRender
  currentCursorIs 3 4
  cursorIs 5 0 ow
  myoPrev
  currentCursorIs 9 2
  cursorIs 0 0 ow

test_outputPrev :: UnitTest
test_outputPrev =
  tmuxTestDef outputPrevTest

outputNextTest :: MyoTest ()
outputNextTest = do
  updateSetting Settings.outputSelectFirst True
  ow <- cycleTestRender
  currentCursorIs 9 2
  cursorIs 0 0 ow
  myoNext
  currentCursorIs 3 4
  cursorIs 5 0 ow

test_outputNext :: UnitTest
test_outputNext =
  tmuxTestDef outputNextTest
