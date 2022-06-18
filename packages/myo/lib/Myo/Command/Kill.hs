module Myo.Command.Kill where

-- import System.Posix.Signals (Signal)
-- import qualified System.Posix.Signals as Signal (killProcess, signalProcess)

-- import Myo.Command.Data.CommandState (CommandState)
-- import Myo.Command.Data.Pid (Pid, pidNum)
-- import Myo.Command.Execution (executionPid, findExecution)

-- signalPid ::
--   MonadIO m =>
--   Signal ->
--   Pid ->
--   m ()
-- signalPid signal =
--   void . tryAny . liftIO . Signal.signalProcess signal . fromIntegral . pidNum

-- killPid ::
--   MonadIO m =>
--   Pid ->
--   m ()
-- killPid =
--   signalPid Signal.killProcess

-- killCommand ::
--   Member (AtomicState Env) r =>
--   MonadIO m =>
--   Ident ->
--   m ()
-- killCommand =
--   traverse_ killPid <=< fmap (>>= executionPid) <$> findExecution
