module Myo.Command.RunningCommand where

import Chiasma.Data.Ident (Ident, identText, sameIdent)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Exception.Lifted (catch)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (preview)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)
import System.Posix.Process (getProcessStatus)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (running)
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunningCommand (RunningCommand(RunningCommand))
import qualified Myo.Log as Log
import Myo.System.Proc (processExists)

findRunningCommand ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe RunningCommand)
findRunningCommand ident =
  gets f
  where
    f :: CommandState -> Maybe RunningCommand
    f = Lens.preview $ CommandState.running . matchIdentL ident

storeRunningCommand ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Pid ->
  m ()
storeRunningCommand ident pid = do
  Log.debug @Text $ "storing running command `" <> identText ident <> "` with pid " <> show pid
  modifyL @CommandState CommandState.running update
  where
    update rcs =
      RunningCommand ident pid : filter (not . sameIdent ident) rcs

removeRunningCommand ::
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removeRunningCommand ident =
  modifyL @CommandState CommandState.running update
  where
    update =
      filter (not . sameIdent ident)

pidAlive ::
  MonadIO m =>
  MonadBaseControl IO m =>
  RunningCommand ->
  m Bool
pidAlive (RunningCommand _ pid) =
  processExists pid

isCommandRunning ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Bool
isCommandRunning =
  maybe (return False) pidAlive <=< findRunningCommand
