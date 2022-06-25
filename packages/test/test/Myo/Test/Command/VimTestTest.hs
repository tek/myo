module Myo.Test.Command.VimTestTest where

import Ribosome.Api.Function (defineFunction)
import Ribosome.Config.Setting (Settings.update)
import Ribosome.Test.Await (awaitEqual_)

import Myo.Command.Parse (commandOutputByName)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Command.Test (myoVimTest, testName)
import Myo.Data.Env (Myo)
import Myo.Settings (testRunner)

mockVimTestFunctions :: FilePath -> Sem r ()
mockVimTestFunctions fname = do
  defineFunction "MyoTestDetermineRunner" ["file"] ["pure 'cat'"]
  defineFunction "MyoTestExecutable" ["runner"] ["pure 'cat'"]
  defineFunction "MyoTestBuildPosition" ["runner", "pos"] ["pure ['" <> toText fname <> "']"]
  defineFunction "MyoTestBuildArgs" ["runner", "args"] ["pure a:args"]

vimTestTest :: Sem r ()
vimTestTest = do
  fname <- fixture "vim-test/file"
  Settings.update testRunner "proc"
  lift do
    mockVimTestFunctions fname
    addSubprocessRunner
    myoVimTest
  awaitEqual_ "content\n" (commandOutputByName testName)

test_vimTest :: UnitTest
test_vimTest =
  tmuxTestDef vimTestTest
