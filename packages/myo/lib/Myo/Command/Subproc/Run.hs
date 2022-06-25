{-# OPTIONS_GHC -Wno-unused-imports #-}
module Myo.Command.Subproc.Run where

import Chiasma.Data.Ident (Ident)
import Conc (timeout)
import qualified Data.Text as Text
import qualified Network.Socket as Socket
import Network.Socket (ShutdownCmd (ShutdownBoth), SockAddr (SockAddrUnix), connect, socketToHandle)
import Path (Abs, Dir, File, Path, toFilePath)
import Path.IO (doesFileExist, removeFile)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Process (SystemProcess, withSystemProcess)
import Ribosome.Api.Path (nvimCwd)
import qualified System.Exit as ExitCode
import qualified System.IO as IOMode (IOMode (WriteMode))
import System.IO (Handle, IOMode (ReadWriteMode, WriteMode))
import System.Process.Typed (
  proc,
  setStderr,
  setStdout,
  setWorkingDir,
  startProcess,
  unsafeProcessHandle,
  useHandleClose,
  waitExitCode,
  )
import qualified Time
import Time (MilliSeconds (MilliSeconds), Seconds (Seconds))

import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.ExecutionState (ExecutionState (..))
import Myo.Command.Data.Pid (Pid (Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError (..))
import Myo.Command.Data.RunTask (RunTask (..), RunTaskDetails (..))
-- import Myo.Command.Execution (setExecutionState)
-- import Myo.Command.Parse (commandOutput)
import Myo.Data.Env (Env)
import Myo.Network.Socket (unixSocket)

waitForSocket ::
  Members [ChronosTime, Race, Embed IO, Stop RunError] r =>
  Path Abs File ->
  Sem r ()
waitForSocket logPath =
  timeout unit (Seconds 3) (Time.while (MilliSeconds 50) (not <$> doesFileExist logPath)) >>= \case
    Right () -> pure ()
    Left () -> stop RunError.SocketFailure

-- TODO original was Socket.Datagram
withSocket ::
  Members [Stop RunError, Resource, Embed IO] r =>
  Path Abs File ->
  (Handle -> Sem r a) ->
  Sem r a
withSocket path use =
  bracket acquire release \ socket ->
    use =<< embed (socketToHandle socket WriteMode)
  where
    acquire = do
      stopEitherAs RunError.SocketFailure =<< tryAny do
        socket <- Socket.socket Socket.AF_UNIX Socket.Stream 0
        socket <$ Socket.connect socket (Socket.SockAddrUnix (toFilePath path))
    release =
      tryAny_ . Socket.close

-- subprocess ::
--   Member (Scoped res (SystemProcess !! err)) r =>
--   Path Abs Dir ->
--   Ident ->
--   Path Abs File ->
--   [Text] ->
--   Sem r ()
-- subprocess cwd ident logPath (cmd : args) =
--   withSystemProcess do
--     undefined

-- subprocess ::
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   Path Abs Dir ->
--   Ident ->
--   Path Abs File ->
--   [Text] ->
--   Sem r ()
-- subprocess cwd ident logPath (cmd : args) = do
--   waitForSocket logPath
--   socket <- unixSocket
--   result <- runExceptT @Error do
--     liftIO $ connect socket (SockAddrUnix (toFilePath logPath))
--     handle <- liftIO $ socketToHandle socket IOMode.WriteMode
--     let stream = useHandleClose handle
--     prc <- startProcess . setStdout stream . setStderr stream . setWorkingDir cwd $ proc (toString cmd) (toString <$> args)
--     pid <- liftIO $ getPid (unsafeProcessHandle prc)
--     setExecutionState ident (maybe Unknown (Tracked . Pid . fromIntegral) pid)
--     check =<< waitExitCode prc
--     setExecutionState ident Stopped
--   reportError' @Error "subproc" result
--   void $ tryAny $ liftIO $ Socket.shutdown socket ShutdownBoth
--   void $ tryAny $ removeFile logPath
--   where
--     check ExitCode.ExitSuccess =
--       unit
--     check (ExitCode.ExitFailure _) = do
--       undefined
--       -- output <- commandOutput ident Nothing False
--       -- stop $ RunError.SubprocFailed (Text.lines output)
-- subprocess _ _ _ _ =
--   stop $ RunError.InvalidCmdline "empty cmdline"

-- runSubproc ::
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   Ident ->
--   Text ->
--   Path Abs File ->
--   Sem r ()
-- runSubproc ident line logPath = do
--   cwd <- nvimCwd
--   void $ async (subprocess cwd ident logPath (Text.words line))

-- runSubprocTask ::
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   RunTask ->
--   Sem r ()
-- runSubprocTask (RunTask (Command _ ident lines' _ _ _ _ _ _) logPath details) =
--   case details of
--     System -> run ident lines'
--     UiSystem _ -> run ident lines'
--     UiShell _ _ -> stop $ RunError.Unsupported "subproc" "shell"
--     Vim -> stop $ RunError.Unsupported "subproc" "vim"
--   where
--     run ident' [line] =
--       runSubproc ident' line logPath
--     run _ _ =
--       stop $ RunError.InvalidCmdline "proc command must have exactly one line"
