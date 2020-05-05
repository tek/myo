module Myo.Command.Subproc.Run where

import Control.Concurrent.Lifted (fork)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text as Text
import Myo.Command.Data.CommandState (CommandState)
import Network.Socket (SockAddr(SockAddrUnix), connect, socketToHandle)
import Path (Abs, File, Path, toFilePath)
import Path.IO (doesFileExist)
import Ribosome.Control.Concurrent.Wait (waitIOPredDef)
import Ribosome.Error.Report (reportError')
import qualified System.Exit as ExitCode
import qualified System.IO as IOMode (IOMode(WriteMode))
import System.Process (getPid)
import System.Process.Typed (
  getStderr,
  getStdout,
  proc,
  setStderr,
  setStdout,
  startProcess,
  unsafeProcessHandle,
  useHandleClose,
  waitExitCode,
  )

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails(..))
import Myo.Command.Parse (commandOutput)
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
  MonadIO m =>
  MonadBase IO m =>
  MonadBaseControl IO m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e RunError m =>
  Ident ->
  TVar (Maybe Pid) ->
  Path Abs File ->
  [Text] ->
  m ()
subprocess ident pidVar logPath (cmd : args) = do
  waitForSocket logPath
  socket <- unixSocket
  liftIO $ connect socket (SockAddrUnix (toFilePath logPath))
  handle <- liftIO $ socketToHandle socket IOMode.WriteMode
  let stream = useHandleClose handle
  prc <- startProcess . setStdout stream . setStderr stream $ proc (toString cmd) (toString <$> args)
  pid <- liftIO $ getPid (unsafeProcessHandle prc)
  atomically $ writeTVar pidVar (Pid . fromIntegral <$> pid)
  check =<< waitExitCode prc
  where
    check ExitCode.ExitSuccess =
      unit
    check (ExitCode.ExitFailure _) = do
      output <- commandOutput ident Nothing
      throwHoist $ RunError.SubprocFailed (Text.lines output)
subprocess _ _ _ _ =
  throwHoist $ RunError.InvalidCmdline "empty cmdline"

runSubproc ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Text ->
  Path Abs File ->
  m ()
runSubproc ident line logPath = do
  pidVar <- liftIO $ newTVarIO Nothing
  void $ fork (run pidVar)
    where
      run pidVar =
        reportError' @Error "subproc" =<< runExceptT (subprocess ident pidVar logPath (Text.words line))

runSubprocTask ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  RunTask ->
  m ()
runSubprocTask (RunTask (Command _ ident lines' _ _ _ _) logPath details) =
  case details of
    System -> run ident lines'
    UiSystem _ -> run ident lines'
    UiShell _ _ -> throwHoist $ RunError.Unsupported "subproc" "shell"
    Vim -> throwHoist $ RunError.Unsupported "subproc" "vim"
  where
    run ident [line] =
      runSubproc ident line logPath
    run _ _ =
      throwHoist $ RunError.InvalidCmdline "proc command must have exactly one line"
