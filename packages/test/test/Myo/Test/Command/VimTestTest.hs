module Myo.Test.Command.VimTestTest where

import Exon (exon)
import Path (Abs, File, Path, relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq)
import Ribosome (Rpc, pathText)
import Ribosome.Api (currentBufferContent, defineFunction)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testError, testHandler)

import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Parse (commandOutputByName, myoParseLatest)
import Myo.Command.VimTest (myoVimTest, testName)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Output.Data.OutputParser (OutputParser (OutputParser))
import Myo.Output.Interpreter.Parsing (interpretParsing)
import Myo.Settings (testLang, testRunner)
import Myo.Test.Embed (myoTest)
import Myo.Test.Output.Echo (echoLang, parseEcho)

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
  myoTest $ interpretBackendProcessNative $ interpretOutputs $ interpretControllerTransient [] do
    fname <- Test.fixturePath [relfile|vim-test/file|]
    Settings.update testRunner "proc"
    Settings.update testLang echoLang
    mockVimTestFunctions fname
    testHandler myoVimTest
    assertWait (testError (commandOutputByName testName)) (assertEq "echoline 1")
    interpretParsing [(echoLang, [OutputParser (parseEcho fname)])] do
      testHandler myoParseLatest
    assertEq ["echoline 1"] =<< currentBufferContent
