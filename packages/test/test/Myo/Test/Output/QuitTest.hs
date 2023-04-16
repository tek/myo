module Myo.Test.Output.QuitTest where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Polysemy.Test (UnitTest, assertEq, evalMaybe)
import Ribosome (Execution (Sync), rpcCommand)
import Ribosome.Api (nvimInput, nvimListWins, nvimSetCurrentWin)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import qualified Ribosome.Scratch as Scratch
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testError, testPluginEmbed)
import Ribosome.Test.Ui (windowCountIs)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage (FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Output.ParseReport (myoOutputQuit, outputQuitName)
import qualified Myo.Settings as Settings
import Myo.Test.Run (runMyoTestStack)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

events :: Vector OutputEventMeta
events =
  Vector.fromList [OutputEventMeta (Just loc) 0, OutputEventMeta (Just loc) 1]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent events messages))

test_outputQuit :: UnitTest
test_outputQuit =
  runMyoTestStack def $ withHandlers [rpcCommand outputQuitName Sync myoOutputQuit] $ testPluginEmbed do
    Settings.update Settings.outputAutoJump False
    storeParseResult "test" [parsedOutput]
    testError compileAndRenderReport
    windowCountIs 2
    win <- fmap (.window) . evalMaybe . head =<< Scratch.get
    nvimSetCurrentWin win
    nvimInput "q"
    assertWait (length <$> nvimListWins) (assertEq 1)
