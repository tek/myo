module Myo.Command.Kill where

import Chiasma.Data.Ident (Ident)
import Control.Monad.Trans.Control (MonadBaseControl)
import Ribosome.Control.Exception (tryAny)
import System.Posix.Signals (Signal)
import qualified System.Posix.Signals as Signal (killProcess, signalProcess)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Pid (Pid, pidNum)
import Myo.Command.Execution (executionPid, findExecution)

signalPid ::
  MonadBaseControl IO m =>
  MonadIO m =>
  Signal ->
  Pid ->
  m ()
signalPid signal =
  void . tryAny . liftIO . Signal.signalProcess signal . fromIntegral . pidNum

killPid ::
  MonadBaseControl IO m =>
  MonadIO m =>
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
