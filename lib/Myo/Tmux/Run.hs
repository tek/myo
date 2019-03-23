module Myo.Tmux.Run where

import Chiasma.Command.Pane (sendKeys)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, local, mapE)
import UnliftIO (MonadUnliftIO)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(Views, Toggle))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.Env (Env, MyoE)
import Myo.Tmux.IO (RunTmux, runMyoTmux)
import qualified Myo.Ui.Data.ToggleError as ToggleError (ToggleError(..))
import Myo.Ui.View (envViewsLens)
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
