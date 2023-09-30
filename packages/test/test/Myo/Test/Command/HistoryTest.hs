module Myo.Test.Command.HistoryTest where

import Data.Aeson (eitherDecode)
import Path (relfile)
import Polysemy.Test (UnitTest, assertEq, assertRight, fixture, runTestAuto, unitTest)
import Ribosome (interpretPersistNull)
import Ribosome.Test (testHandler)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Run (myoLineCmd)
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Default (setupDefaultTestUi)
import Test.Tasty (TestTree, testGroup)

test_historyNub :: UnitTest
test_historyNub =
  myoEmbedTmuxTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendProcessNative $
  interpretController $ testHandler do
    setupDefaultTestUi
    myoLineCmd "echo 1"
    myoLineCmd "echo 2"
    myoLineCmd "echo 2"
    myoLineCmd "echo 3"
    assertEq 3 . length =<< atomicView @CommandState #history

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
