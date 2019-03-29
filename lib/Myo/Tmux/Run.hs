module Myo.Tmux.Run where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View.State as Views (paneId)
import Control.Monad.DeepError (MonadDeepError, throwHoist)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Ribosome.Control.Concurrent.Wait (waitIODef)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import UnliftIO.Directory (doesPathExist)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(SocketFailure))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (pipePaneToSocket)
import Myo.Command.Watch (watchPane)
import Myo.Data.Env (Env)
import Myo.Tmux.IO (RunTmux, runMyoTmux)

tmuxCanRun :: RunTask -> Bool
tmuxCanRun (RunTask _ _ details) =
  checkCmd details
  where
    checkCmd (RunTaskDetails.UiSystem _) = True
    checkCmd (RunTaskDetails.UiShell _ _) = True
    checkCmd _ = False

waitForSocket :: (MonadIO m, MonadBaseControl IO m, MonadDeepError e RunError m) => FilePath -> m ()
waitForSocket logPath =
  waitIODef (pure logPath) doesPathExist >>= \case
    Right _ -> return ()
    Left _ -> throwHoist RunError.SocketFailure

tmuxRun ::
  (MonadRibo m, MonadIO m, MonadBaseControl IO m, RunTmux m, MonadDeepState s CommandState m, MonadDeepState s Views m, MonadDeepError e ViewsError m, MonadDeepError e RunError m) =>
  RunTask ->
  m ()
tmuxRun (RunTask (Command _ commandIdent lines' _ _) logPath details) =
  run details
  where
    run (RunTaskDetails.UiSystem paneIdent) = do
      paneId <- Views.paneId paneIdent
      watchPane commandIdent logPath
      waitForSocket logPath
      runMyoTmux $ do
        pipePaneToSocket paneId logPath
        sendKeys paneId lines'
    run _ = undefined
