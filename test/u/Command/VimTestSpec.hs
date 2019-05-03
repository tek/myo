{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Command.VimTestSpec (htf_thisModulesTests) where

import qualified Data.Text as Text (intercalate)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Nvim.Api.IO (vimCallFunction, vimCommand)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)
import Ribosome.Test.Unit (fixture, withLog)
import Test.Framework

import Myo.Command.Parse (commandOutput)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Command.Test (myoVimTest, testIdent)
import Myo.Data.Env (Myo)
import Myo.Settings (testRunner)
import Unit (specDef)

defineFunction ::
  NvimE e m =>
  Text ->
  [Text] ->
  [Text] ->
  m ()
defineFunction name params body =
  vimCommand $ unlines $ sig : body ++ ["endfunction"]
  where
    sig =
      "function! " <> name <> "(" <> Text.intercalate ", " params <> ")"

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
  await (gassertEqual "content\n") (commandOutput testIdent)

test_vimTest :: IO ()
test_vimTest =
  specDef (withLog vimTestSpec)
