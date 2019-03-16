module Myo.Tmux.Run where

import Chiasma.Command.Pane (sendKeys)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.Catch (MonadThrow)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, local, mapE)
import UnliftIO (MonadUnliftIO)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(Views, Toggle))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.Env (MyoE)
import Myo.Tmux.IO (myoTmux)
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

tmuxRun :: (Nvim m, MonadRibo m, MonadUnliftIO m, MonadThrow m) => RunTask -> MyoE RunError m ()
tmuxRun (RunTask (Command _ _ lines' _ _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      paneId <- mapE RunError.Views $ local envViewsLens $ Views.paneId paneIdent
      watchPane paneIdent logPath
      mapE (RunError.Toggle . ToggleError.Render . RenderError.Fatal) $ myoTmux $ do
        pipePaneToSocket paneId logPath
        sendKeys paneId lines'
    run _ = undefined
