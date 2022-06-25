module Myo.Command.Kill where

import Chiasma.Data.Ident (Ident)
import System.Posix.Signals (Signal)
import qualified System.Posix.Signals as Signal (killProcess, signalProcess)

import Myo.Command.Data.Pid (Pid)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Execution (executionPid)

signalPid ::
  Member (Embed IO) r =>
  Signal ->
  Pid ->
  Sem r ()
signalPid signal =
  void . tryAny . liftIO . Signal.signalProcess signal . fromIntegral

killPid ::
  Member (Embed IO) r =>
  Pid ->
  Sem r ()
killPid =
  signalPid Signal.killProcess

killCommand ::
  Member (Embed IO) r =>
  Member Executions r =>
  Ident ->
  Sem r ()
killCommand =
  traverse_ killPid <=< fmap (>>= executionPid) <$> Executions.get
