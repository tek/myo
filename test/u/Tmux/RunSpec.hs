{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.RunSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Test.Tmux (sleep)
import qualified Control.Lens as Lens (view, at, preview, each)
import Control.Monad.IO.Class (liftIO)
import Ribosome.Control.Monad.RiboE (runRiboE, liftRibo)
import Ribosome.Control.Monad.State (riboState, runRiboStateE)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import Ribosome.Error.Report (printAllErrors)
import Ribosome.Msgpack.NvimObject ((-$))
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Myo, MyoE, Runner)
import qualified Myo.Data.Env as Env (_errors)
import Myo.Test.Unit (tmuxGuiSpecWithDef)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (setupDefaultUi)

runSpec :: Myo ()
runSpec = do
  let ident = Str "cmd"
  addTmuxRunner
  myoAddSystemCommand -$ AddSystemCommandOptions ident ["ls"] (Just (Str "tmux")) (Just (Str "make"))
  myoRun -$ ident
  printAllErrors
  liftIO $ assertEqual "" ""

test_tmuxRun :: IO ()
test_tmuxRun =
  vars >>= tmuxGuiSpecWithDef runSpec
