module Myo.Command.RunTask where

import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Ui.Render (MyoRender)

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import Myo.Command.Log (commandLogPath)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)

runTask ::
  MonadIO m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MyoRender s e m =>
  MonadThrow m =>
  Command ->
  m RunTask
runTask cmd = do
  details <- runDetails cmd
  cmdLog <- commandLogPath (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
