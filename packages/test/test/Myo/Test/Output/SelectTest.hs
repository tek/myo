module Myo.Test.Output.SelectTest where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, evalMaybe)
import Ribosome (pathText)
import Ribosome.Api (nvimInput, nvimSetCurrentWin)
import Ribosome.Api.Window (currentCursor)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import qualified Ribosome.Scratch as Scratch
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testError, testPluginEmbed)
import Ribosome.Test.Ui (windowCountIs)

import Myo.Command.Data.OutputState (OutputState (OutputState))
import Myo.Command.Output (compileAndRenderReport)
import qualified Myo.Effect.Outputs as Outputs
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents)
import Myo.Output.Lang.Haskell.Report (HaskellMessage (FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Plugin (outputMappingHandlers)
import qualified Myo.Settings as Settings
import Myo.Test.Run (runMyoTestStack)

events :: Text -> Vector OutputEventMeta
events file =
  Vector.fromList [OutputEventMeta (Just (Location file 9 (Just 2))) 0, OutputEventMeta Nothing 1]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: Text -> OutputEvents
parsedOutput file =
  parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent (events file) messages)

test_outputSelect :: UnitTest
test_outputSelect =
  runMyoTestStack def $ interpretOutputs $ withHandlers outputMappingHandlers $ testPluginEmbed do
    Settings.update Settings.outputAutoJump False
    Settings.update Settings.outputSelectFirst True
    file <- Test.fixturePath [relfile|output/select/File.hs|]
    let po = parsedOutput (pathText file)
    Outputs.setCurrentOutput (OutputState "test" "test" [haskellSyntax] po def Nothing)
    testError @OutputError compileAndRenderReport
    windowCountIs 2
    win <- fmap (.window) . evalMaybe . head =<< Scratch.get
    nvimSetCurrentWin win
    nvimInput "<cr>"
    assertWait currentCursor (assertEq (9, 2))
