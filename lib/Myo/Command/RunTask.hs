module Myo.Command.RunTask where

import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Myo.Ui.Render (MyoRender)

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (commandLog)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Data.Env (Env)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Toggle (ensurePaneOpen)

ensurePrerequisites ::
  (
    MonadIO m,
    MonadDeepError e TreeModError (t m),
    MonadDeepError e ToggleError (t m),
    MonadDeepState s Env m,
    MyoRender s e t m
  ) =>
  RunTaskDetails ->
  t m ()
ensurePrerequisites RunTaskDetails.Vim = return ()
ensurePrerequisites (RunTaskDetails.UiSystem ident) =
  ensurePaneOpen ident
ensurePrerequisites _ = undefined

runTask ::
  (
    MonadIO m,
    MonadDeepError e TreeModError (t m),
    MonadDeepError e RunError (t m),
    MonadDeepError e ToggleError (t m),
    MonadDeepState s Env (t m),
    MyoRender s e t m
  ) =>
  Command ->
  t m RunTask
runTask cmd = do
  details <- runDetails cmd
  ensurePrerequisites details
  cmdLog <- commandLog (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
