module Myo.Command.Subproc.Runner where

import Control.Monad.Trans.Control (MonadBaseControl)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails(System, UiSystem))
import Myo.Command.Runner (addRunner, mkRunner, RunInIO)
import Myo.Command.Subproc.Run (runSubprocTask)
import Myo.Data.Env (Env)

subprocCanRun :: RunTask -> Bool
subprocCanRun (RunTask _ _ System) =
  True
subprocCanRun (RunTask _ _ (UiSystem _)) =
  True
subprocCanRun _ =
  False

addSubprocessRunner ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  RunInIO m =>
  m ()
addSubprocessRunner =
  addRunner "proc" (mkRunner runSubprocTask) subprocCanRun
