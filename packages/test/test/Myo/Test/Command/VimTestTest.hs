module Myo.Test.Command.VimTestTest where

import Ribosome.Api.Function (defineFunction)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Myo.Command.Parse (commandOutputByName)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Command.Test (myoVimTest, testName)
import Myo.Data.Env (Myo)
import Myo.Settings (testRunner)
import Myo.Test.Unit (MyoTest, testDef)

mockVimTestFunctions :: FilePath -> Myo ()
mockVimTestFunctions fname = do
  defineFunction "MyoTestDetermineRunner" ["file"] ["return 'cat'"]
  defineFunction "MyoTestExecutable" ["runner"] ["return 'cat'"]
  defineFunction "MyoTestBuildPosition" ["runner", "pos"] ["return ['" <> toText fname <> "']"]
  defineFunction "MyoTestBuildArgs" ["runner", "args"] ["return a:args"]

vimTestTest :: MyoTest ()
vimTestTest = do
  fname <- fixture "vim-test/file"
  updateSetting testRunner "proc"
  lift do
    mockVimTestFunctions fname
    addSubprocessRunner
    myoVimTest
  awaitEqual_ "content\n" (commandOutputByName testName)

test_vimTest :: UnitTest
test_vimTest =
  testDef vimTestTest
