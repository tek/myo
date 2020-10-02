module Myo.Command.RunTask where

import qualified Control.Lens as Lens (view)
import Myo.Command.Data.CommandError (CommandError)

import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.Command as Command (ident)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..))
import Myo.Command.Log (commandLogPath)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Data.Env (Env)

runTask ::
  MonadIO m =>
  MonadDeepError e RunError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  Command ->
  m RunTask
runTask cmd = do
  details <- runDetails cmd
  cmdLog <- commandLogPath (Lens.view Command.ident cmd)
  return $ RunTask cmd cmdLog details
