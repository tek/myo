module Myo.Command.Kill where

import Ribosome.Control.Exception (tryAny)
import System.Posix.Signals (Signal)
import qualified System.Posix.Signals as Signal (killProcess, signalProcess)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Pid (Pid, pidNum)
import Myo.Command.Execution (executionPid, findExecution)

signalPid ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Signal ->
  Pid ->
  m ()
signalPid signal =
  void . tryAny . liftIO . Signal.signalProcess signal . fromIntegral . pidNum

killPid ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  m ()
killPid =
  signalPid Signal.killProcess

killCommand ::
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadIO m =>
  Ident ->
  m ()
killCommand =
  traverse_ killPid <=< (>>= executionPid) <$$> findExecution
