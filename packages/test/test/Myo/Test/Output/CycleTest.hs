module Myo.Test.Output.CycleTest where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, Test, TestError, UnitTest)
import Ribosome (Rpc, RpcError, Scratch, Settings, Window, pathText)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (testError, testHandler)
import Ribosome.Test.Ui (currentCursorIs, cursorIs, windowCountIs)

import Myo.Command.Output (compileAndRenderReport, myoNext, myoPrev)
import Myo.Command.Parse (storeParseResult)
import Myo.Effect.Outputs (Outputs)
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage (FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Output.ParseReport (outputWindow)
import qualified Myo.Settings as Settings (outputSelectFirst)
import Myo.Test.Embed (myoTest)

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

cycleTestRender ::
  Members [Outputs, Hedgehog IO, Test, Error TestError] r =>
  Members [Scratch, Rpc, Rpc !! RpcError, Settings, Embed IO] r =>
  Sem r Window
cycleTestRender = do
  file <- Test.fixturePath [relfile|output/select/File.hs|]
  let po = [parsedOutput (pathText file)]
  storeParseResult "test" "test" po
  testError @OutputError compileAndRenderReport
  windowCountIs 2
  testError outputWindow

test_outputPrev :: UnitTest
test_outputPrev =
  myoTest $ interpretOutputs $ testHandler do
    Settings.update Settings.outputSelectFirst False
    ow <- cycleTestRender
    currentCursorIs 3 4
    cursorIs 5 0 ow
    myoPrev
    currentCursorIs 9 2
    cursorIs 0 0 ow

test_outputNext :: UnitTest
test_outputNext =
  myoTest $ interpretOutputs $ testHandler do
    Settings.update Settings.outputSelectFirst True
    ow <- cycleTestRender
    currentCursorIs 9 2
    cursorIs 0 0 ow
    myoNext
    currentCursorIs 3 4
    cursorIs 5 0 ow
