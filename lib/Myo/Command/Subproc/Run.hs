module Myo.Command.Subproc.Run where

import Control.Concurrent.Lifted (fork)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Text as Text (words)
import Myo.Command.Data.CommandState (CommandState)
import Network.Socket (SockAddr(SockAddrUnix), connect, socketToHandle)
import Path (Abs, File, Path, toFilePath)
import qualified System.IO as IOMode (IOMode(WriteMode))
import System.Process (getPid)
import System.Process.Typed (
  proc,
  setStderr,
  setStdout,
  startProcess,
  unsafeProcessHandle,
  useHandleClose,
  waitExitCode,
  )

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.Pid (Pid(Pid))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails(..))
import Myo.Network.Socket (unixSocket)

subprocess ::
  MonadIO m =>
  MonadBase IO m =>
  MonadDeepError e RunError m =>
  TVar (Maybe Pid) ->
  Path Abs File ->
  [Text] ->
  m ()
subprocess pidVar logPath (cmd : args) = do
  socket <- unixSocket
  liftIO $ connect socket (SockAddrUnix (toFilePath logPath))
  handle <- liftIO $ socketToHandle socket IOMode.WriteMode
  let stream = useHandleClose handle
  prc <- startProcess . setStdout stream . setStderr stream $ proc (toString cmd) (toString <$> args)
  pid <- liftIO $ getPid (unsafeProcessHandle prc)
  atomically $ writeTVar pidVar (Pid . fromIntegral <$> pid)
  void $ waitExitCode prc
subprocess _ _ _ =
  throwHoist $ RunError.InvalidCmdline "empty cmdline"

runSubproc ::
  MonadRibo m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s CommandState m =>
  Text ->
  Path Abs File ->
  m ()
runSubproc line logPath = do
  pidVar <- liftIO $ newTVarIO Nothing
  void . fork $ subprocess pidVar logPath (Text.words line)

runSubprocTask ::
  MonadRibo m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonadDeepError e RunError m =>
  MonadDeepState s CommandState m =>
  RunTask ->
  m ()
runSubprocTask (RunTask (Command _ _ lines' _ _ _ _) logPath details) =
  case details of
    System -> run lines'
    UiSystem _ -> run lines'
    UiShell _ _ -> throwHoist $ RunError.Unsupported "subproc" "shell"
    Vim -> throwHoist $ RunError.Unsupported "subproc" "vim"
  where
    run [line] =
      runSubproc line logPath
    run _ =
      throwHoist $ RunError.InvalidCmdline "proc command must have exactly one line"
