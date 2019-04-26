module Myo.Command.Log where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Control.Lens (Lens', (?~))
import qualified Control.Lens as Lens (at, over)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Free (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (null)
import qualified Data.Map as Map (keys)
import Network.Socket (Socket)
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath, (</>))

import Myo.Command.Command (mainCommand)
import Myo.Command.Data.CommandLog (CommandLog(CommandLog))
import Myo.Command.Data.CommandState (CommandState, Logs)
import qualified Myo.Command.Data.CommandState as CommandState (logPaths, logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (tempDir)
import Myo.Network.Socket (socketBind)

logPathLens :: Ident -> Lens' CommandState (Maybe (Path Abs File))
logPathLens ident = CommandState.logPaths . Lens.at ident

logPathByIdent ::
  (MonadDeepError e RunError m, MonadDeepState s CommandState m) =>
  Ident ->
  m (Maybe (Path Abs File))
logPathByIdent ident =
  getL $ logPathLens ident

logTempDir ::
  MonadDeepState s Env m =>
  m (Path Abs Dir)
logTempDir =
  getL @Env Env.tempDir

insertLogPath ::
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadThrow m =>
  Ident ->
  m (Path Abs File)
insertLogPath ident = do
  base <- logTempDir
  fileName <- parseRelFile $ "pane-" <> identString ident
  let logPath = base </> fileName
  modify $ logPathLens ident ?~ logPath
  return logPath

commandLogPath ::
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadThrow m =>
  Ident ->
  m (Path Abs File)
commandLogPath ident = do
  existing <- logPathByIdent ident
  maybe (insertLogPath ident) return existing

commandLogSocketBind ::
  MonadDeepError e RunError m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadThrow m =>
  MonadBase IO m =>
  Ident ->
  m Socket
commandLogSocketBind =
  socketBind <=< commandLogPath

pipePaneToSocket ::
  (MonadIO m, MonadFree TmuxThunk m) =>
  PaneId ->
  Path Abs File ->
  m ()
pipePaneToSocket paneId logPath =
  pipePane paneId cmd
  where
    cmd = "'socat STDIN UNIX-SENDTO:" <> toFilePath logPath <> "'"

logLens :: Ident -> Lens' CommandState (Maybe CommandLog)
logLens ident = CommandState.logs . Lens.at ident

commandLogs :: MonadDeepState s CommandState m => m Logs
commandLogs =
  getL @CommandState CommandState.logs

commandLog ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe CommandLog)
commandLog ident = do
  logIdent <- mainCommand ident
  getL (logLens logIdent)

appendLog ::
  MonadDeepState s CommandState m =>
  Ident ->
  ByteString ->
  m ()
appendLog ident bytes =
  modify $ Lens.over (logLens ident) append
  where
    append (Just (CommandLog prev cur)) = Just (CommandLog prev (cur <> bytes))
    append Nothing = Just (CommandLog [] bytes)

pushCommandLog ::
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
pushCommandLog ident =
  modifyL (logLens ident) (fmap push)
  where
    push (CommandLog prev cur) | ByteString.null cur =
      CommandLog prev cur
    push (CommandLog prev cur) =
      CommandLog (cur : prev) ""

pushCommandLogs ::
  MonadDeepState s CommandState m =>
  m ()
pushCommandLogs = do
  idents <- getsL @CommandState CommandState.logs Map.keys
  traverse_ pushCommandLog idents
