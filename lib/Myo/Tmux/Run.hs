module Myo.Tmux.Run(
  tmuxCanRun,
  tmuxRun,
) where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.View (View(View))
import Chiasma.View (pane)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Ribosome.Control.Monad.RiboE (liftRibo, mapE)
import Ribosome.Control.Monad.State (riboStateLocalE)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import qualified Chiasma.View.State as Views (paneId)

import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(Views, Tmux))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Data.Env (MyoE)
import Myo.Ui.View (envViewsLens)
import Myo.Tmux.IO (myoTmux)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

tmuxRun :: RunTask -> MyoE RunError (Maybe Pid)
tmuxRun (RunTask (Command _ _ lines' _) _ details) = do
  run details
  return Nothing
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      paneId <- mapE RunError.Views $ riboStateLocalE envViewsLens $ Views.paneId paneIdent
      mapE RunError.Tmux $ myoTmux $ sendKeys paneId lines'
