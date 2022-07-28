module Myo.Test.Command.HistoryTest where

import Polysemy.Test (UnitTest, assertEq)
import Ribosome (interpretPersistNull)
import Ribosome.Test (testHandler)

import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Run (myoLineCmd)
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)

test_history :: UnitTest
test_history =
  myoEmbedTmuxTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendProcessNative $
  interpretController $ testHandler do
    setupDefaultTestUi
    myoLineCmd "echo 1"
    myoLineCmd "echo 2"
    myoLineCmd "echo 2"
    myoLineCmd "echo 3"
    assertEq 3 . length =<< atomicView @CommandState #history
