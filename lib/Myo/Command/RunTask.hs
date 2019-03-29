module Myo.Command.RunTask where

import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Myo.Ui.Render (MyoRender)

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (commandLogPath)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Toggle (ensurePaneOpen)

ensurePrerequisites ::
  (
    MonadIO m,
    MonadDeepState s Env m,
    MonadDeepError e ToggleError m,
    MonadDeepError e TreeModError m,
    MyoRender s e m
  ) =>
  RunTaskDetails ->
  m ()
ensurePrerequisites RunTaskDetails.Vim = return ()
ensurePrerequisites (RunTaskDetails.UiSystem ident) =
  ensurePaneOpen ident
ensurePrerequisites _ = undefined

runTask ::
  (
    MonadIO m,
    MonadDeepError e TreeModError m,
    MonadDeepError e RunError m,
    MonadDeepError e ToggleError m,
    MonadDeepState s Env m,
    MonadDeepState s CommandState m,
    MyoRender s e m
  ) =>
  Command ->
  m RunTask
runTask cmd = do
  details <- runDetails cmd
  ensurePrerequisites details
  cmdLog <- commandLogPath (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
