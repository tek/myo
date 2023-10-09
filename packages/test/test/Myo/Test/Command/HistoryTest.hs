module Myo.Test.Command.HistoryTest where

import Data.Aeson (eitherDecode)
import Path (relfile)
import Polysemy.Test (UnitTest, assertEq, assertRight, fixture, runTestAuto, unitTest)
import Ribosome.Test (resumeTestError, testHandler)
import Test.Tasty (TestTree, testGroup)

import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Run (myoLineCmd)
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Default (setupDefaultTestUi)

test_historyNub :: UnitTest
test_historyNub =
  myoEmbedTmuxTest $ interpretBackendProcessNative $ interpretControllerTransient [] $ testHandler do
    setupDefaultTestUi
    myoLineCmd "echo 1"
    myoLineCmd "echo 2"
    myoLineCmd "echo 2"
    myoLineCmd "echo 3"
    assertEq 3 . length =<< resumeTestError @History History.all

test_historyCodec :: UnitTest
test_historyCodec =
  runTestAuto do
    content <- fixture [relfile|history.json|]
    assertRight 3 (length <$> eitherDecode @[HistoryEntry] (encodeUtf8 content))

test_history :: TestTree
test_history =
  testGroup "history" [
    unitTest "add history entries when running commands, unique by command lines" test_historyNub,
    unitTest "backwards compatible json parsing" test_historyCodec
  ]
