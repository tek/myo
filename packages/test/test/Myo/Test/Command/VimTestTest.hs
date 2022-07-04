module Myo.Test.Command.VimTestTest where

import Path (Abs, File, Path, relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq)
import Ribosome.Api (defineFunction)
import qualified Ribosome.Settings as Settings

import Myo.Settings (testRunner)
import Myo.Test.Embed (myoTest)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Interpreter.Controller (interpretController)
import Ribosome (interpretPersistNull, Rpc, pathText)
import Ribosome.Test (assertWait, testHandler, testError)
import Myo.Command.Parse (commandOutputByName)
import Myo.Command.Test (myoVimTest, testName)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Exon (exon)

mockVimTestFunctions ::
  Member (Rpc) r =>
  Path Abs File ->
  Sem r ()
mockVimTestFunctions fname = do
  defineFunction "MyoTestDetermineRunner" ["file"] ["return 'cat'"]
  defineFunction "MyoTestExecutable" ["runner"] ["return 'cat'"]
  defineFunction "MyoTestBuildPosition" ["runner", "pos"] [[exon|return ['#{pathText fname}']|]]
  defineFunction "MyoTestBuildArgs" ["runner", "args"] ["return a:args"]

test_vimTest :: UnitTest
test_vimTest =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendProcessNative $ interpretController do
    fname <- Test.fixturePath [relfile|vim-test/file|]
    Settings.update testRunner "proc"
    mockVimTestFunctions fname
    testHandler myoVimTest
    assertWait (testError (commandOutputByName testName)) (assertEq "content")
