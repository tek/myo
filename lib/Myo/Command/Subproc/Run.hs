module Myo.Command.Subproc.Run where

import Control.Concurrent.Lifted (fork)
import qualified Data.Text as Text
import Myo.Command.Data.CommandState (CommandState)
import qualified Network.Socket as Socket
import Network.Socket (ShutdownCmd(ShutdownBoth), SockAddr(SockAddrUnix), connect, socketToHandle)
import Path (Abs, File, Path, toFilePath)
import Path.IO (doesFileExist, removeFile)
import Ribosome.Api.Path (nvimCwd)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Control.Exception (tryAny)
import Ribosome.Error.Report (reportError')
import qualified System.Exit as ExitCode
import qualified System.IO as IOMode (IOMode(WriteMode))
import System.Process (getPid)
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

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.Execution (ExecutionState(..))
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails(..))
import Myo.Command.Execution (setExecutionState)
import Myo.Command.Parse (commandOutput)
import Myo.Data.Env (Env)
import Myo.Data.Error (Error)
import Myo.Network.Socket (unixSocket)
import Myo.Output.Data.OutputError (OutputError)

waitForSocket ::
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  Path Abs File ->
  m ()
waitForSocket logPath =
  waitIOPredDef (pure logPath) doesFileExist >>= \case
    Right _ -> return ()
    Left _ -> throwHoist RunError.SocketFailure

subprocess ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e RunError m =>
  String ->
  Ident ->
  Path Abs File ->
  [Text] ->
  m ()
subprocess cwd ident logPath (cmd : args) = do
  waitForSocket logPath
  socket <- unixSocket
  result <- runExceptT @Error $ do
    liftIO $ connect socket (SockAddrUnix (toFilePath logPath))
    handle <- liftIO $ socketToHandle socket IOMode.WriteMode
    let stream = useHandleClose handle
    prc <- startProcess . setStdout stream . setStderr stream . setWorkingDir cwd $ proc (toString cmd) (toString <$> args)
    pid <- liftIO $ getPid (unsafeProcessHandle prc)
    setExecutionState ident (maybe Unknown (Tracked . Pid . fromIntegral) pid)
    check =<< waitExitCode prc
    setExecutionState ident Stopped
  reportError' @Error "subproc" result
  void $ tryAny $ liftIO $ Socket.shutdown socket ShutdownBoth
  void $ tryAny $ removeFile logPath
  where
    check ExitCode.ExitSuccess =
      unit
    check (ExitCode.ExitFailure _) = do
      output <- commandOutput ident Nothing False
      throwHoist $ RunError.SubprocFailed (Text.lines output)
subprocess _ _ _ _ =
  throwHoist $ RunError.InvalidCmdline "empty cmdline"

runSubproc ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e RunError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Text ->
  Path Abs File ->
  m ()
runSubproc ident line logPath = do
  cwd <- nvimCwd
  void $ fork (subprocess cwd ident logPath (Text.words line))

runSubprocTask ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e RunError m =>
  MonadDeepState s CommandState m =>
  RunTask ->
  m ()
runSubprocTask (RunTask (Command _ ident lines' _ _ _ _ _ _) logPath details) =
  case details of
    System -> run ident lines'
    UiSystem _ -> run ident lines'
    UiShell _ _ -> throwHoist $ RunError.Unsupported "subproc" "shell"
    Vim -> throwHoist $ RunError.Unsupported "subproc" "vim"
  where
    run ident' [line] =
      runSubproc ident' line logPath
    run _ _ =
      throwHoist $ RunError.InvalidCmdline "proc command must have exactly one line"
