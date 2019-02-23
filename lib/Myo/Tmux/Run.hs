module Myo.Tmux.Run(
  tmuxCanRun,
  tmuxRun,
) where

import Chiasma.Command.Pane (sendKeys)
import qualified Chiasma.View.State as Views (paneId)
import Ribosome.Control.Monad.RiboE (mapE, anaE)
import Ribosome.Control.Monad.State (riboStateLocalE)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(Views, Tmux))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.Env (MyoE)
import Myo.Tmux.IO (myoTmux)
import Myo.Ui.View (envViewsLens)
import Myo.Ui.Watch (watchPane)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

tmuxRun :: RunTask -> MyoE RunError ()
tmuxRun (RunTask (Command _ _ lines' _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      paneId <- mapE RunError.Views $ riboStateLocalE envViewsLens $ Views.paneId paneIdent
      watchPane paneIdent logPath
      anaE RunError.Tmux $ myoTmux $ do
        pipePaneToSocket paneId logPath
        sendKeys paneId lines'
    run _ = undefined
