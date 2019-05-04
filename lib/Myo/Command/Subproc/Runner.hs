module Myo.Command.Subproc.Runner where

import Control.Monad.Trans.Control (MonadBaseControl)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (ExecutionState)
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Unknown))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails(System, UiSystem))
import Myo.Command.Runner (RunInIO, addRunner, extractRunError)
import Myo.Command.Subproc.Run (runSubprocTask)
import Myo.Data.Env (Env)

subprocCanRun :: RunTask -> Bool
subprocCanRun (RunTask _ _ System) =
  True
subprocCanRun (RunTask _ _ (UiSystem _)) =
  True
subprocCanRun _ =
  False

subprocCheckPending ::
  Monad m =>
  RunTask ->
  m (Either RunError (IO ExecutionState))
subprocCheckPending _ =
  return . Right . return $ ExecutionState.Unknown

addSubprocessRunner ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  RunInIO m =>
  m ()
addSubprocessRunner =
  addRunner "proc" (extractRunError runSubprocTask) subprocCheckPending subprocCanRun
