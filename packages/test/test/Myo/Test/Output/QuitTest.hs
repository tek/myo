module Myo.Test.Output.QuitTest where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Polysemy.Test (UnitTest, evalMaybe, assertEq)
import Ribosome (interpretNvimPlugin)
import Ribosome.Api (nvimFeedkeys, nvimSetCurrentWin, nvimListWins)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (testError, testPluginEmbed, assertWait)
import Ribosome.Test.Ui (windowCountIs)

import Myo.Command.Output (compileAndRenderReport, myoOutputQuit)
import Myo.Command.Parse (storeParseResult)
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent (LangOutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage (FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import qualified Myo.Settings as Settings
import Myo.Test.Run (runMyoTestStack)
import qualified Ribosome.Scratch as Scratch

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
  runMyoTestStack def $ interpretNvimPlugin mempty [("output-quit", myoOutputQuit)] mempty $ testPluginEmbed do
    Settings.update Settings.outputAutoJump False
    storeParseResult "test" [parsedOutput]
    testError compileAndRenderReport
    windowCountIs 2
    win <- fmap Scratch.window . evalMaybe . head =<< Scratch.get
    nvimSetCurrentWin win
    nvimFeedkeys "q" "m" True
    assertWait (length <$> nvimListWins) (assertEq 1)
