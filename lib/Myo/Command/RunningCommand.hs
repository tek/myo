module Myo.Command.RunningCommand where

import Chiasma.Data.Ident (Ident, identText, sameIdent)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import qualified Control.Lens as Lens (preview)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Ribosome.Control.Monad.Ribo as Ribo (prepend)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (pendingCommands, running)
import Myo.Command.Data.PendingCommand (PendingCommand)
import Myo.Command.Data.Pid (Pid)
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
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removeRunningCommand ident = do
  Log.debug @Text $ "removing running command `" <> identText ident <> "`"
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

addPendingCommand ::
  MonadDeepState s CommandState m =>
  PendingCommand ->
  m ()
addPendingCommand =
  Ribo.prepend @CommandState CommandState.pendingCommands

removePendingCommand ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removePendingCommand ident = do
  Log.debug @Text $ "removing pending command `" <> identText ident <> "`"
  modifyL @CommandState CommandState.pendingCommands (filter (not . sameIdent ident))
