module Myo.Tmux.Run where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.Env (Env)
import Myo.Tmux.IO (RunTmux, runMyoTmux)
import Myo.Ui.Watch (watchPane)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

tmuxRun ::
  (MonadIO m, MonadBaseControl IO m, RunTmux m, MonadDeepState s Env m, MonadDeepState s Views m, MonadDeepError e ViewsError m) =>
  RunTask ->
  m ()
tmuxRun (RunTask (Command _ _ lines' _ _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      paneId <- Views.paneId paneIdent
      watchPane paneIdent logPath
      runMyoTmux $ do
        pipePaneToSocket paneId logPath
        sendKeys paneId lines'
    run _ = undefined
