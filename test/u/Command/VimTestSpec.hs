{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Command.VimTestSpec (htf_thisModulesTests) where

import qualified Data.Text as Text (intercalate)
import Ribosome.Api.Function (defineFunction)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Await (await)
import Ribosome.Test.Unit (fixture, withLog)
import Test.Framework

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (logs)
import Myo.Command.Parse (commandOutputByName)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Command.Test (myoVimTest, testName)
import Myo.Data.Env (Myo)
import Myo.Settings (testRunner)
import Unit (specDef)

mockVimTestFunctions :: FilePath -> Myo ()
mockVimTestFunctions fname = do
  defineFunction "MyoTestDetermineRunner" ["file"] ["return 'cat'"]
  defineFunction "MyoTestExecutable" ["runner"] ["return 'cat'"]
  defineFunction "MyoTestBuildPosition" ["runner", "pos"] ["return ['" <> toText fname <> "']"]
  defineFunction "MyoTestBuildArgs" ["runner", "args"] ["return a:args"]

vimTestSpec :: Myo ()
vimTestSpec = do
  fname <- fixture "vim-test/file"
  updateSetting testRunner "proc"
  mockVimTestFunctions fname
  addSubprocessRunner
  myoVimTest
  cmds <- getL @CommandState CommandState.logs
  await (gassertEqual "content\n") (commandOutputByName testName)

test_vimTest :: IO ()
test_vimTest =
  specDef (withLog vimTestSpec)
