module Myo.Command.Log where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Control.Lens (Lens', (?~))
import qualified Control.Lens as Lens (at, over)
import Control.Monad.Base (MonadBase)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState, getsL, modify)
import Control.Monad.Free (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Network.Socket (Socket)
import System.FilePath ((</>))

import Myo.Command.Data.CommandLog (CommandLog(CommandLog))
import Myo.Command.Data.CommandState (CommandState, Logs)
import qualified Myo.Command.Data.CommandState as CommandState (logPaths, logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (tempDir)
import Myo.Network.Socket (socketBind)

logPathLens :: Ident -> Lens' CommandState (Maybe FilePath)
logPathLens ident = CommandState.logPaths . Lens.at ident

logPathByIdent ::
  (MonadDeepError e RunError m, MonadDeepState s CommandState m) =>
  Ident ->
  m (Maybe FilePath)
logPathByIdent ident =
  getsL $ logPathLens ident

logTempDir ::
  MonadDeepState s Env m =>
  m FilePath
logTempDir =
  getsL @Env Env.tempDir

insertLogPath ::
  (MonadDeepError e RunError m, MonadDeepState s Env m, MonadDeepState s CommandState m) =>
  Ident ->
  m FilePath
insertLogPath ident = do
  base <- logTempDir
  let logPath = base </> "pane-" ++ identString ident
  modify $ logPathLens ident ?~ logPath
  return logPath

commandLogPath ::
  (MonadDeepError e RunError m, MonadDeepState s Env m, MonadDeepState s CommandState m) =>
  Ident ->
  m FilePath
commandLogPath ident = do
  existing <- logPathByIdent ident
  maybe (insertLogPath ident) return existing

commandLogSocketBind ::
  (MonadDeepError e RunError m, MonadDeepState s Env m, MonadDeepState s CommandState m, MonadBase IO m) =>
  Ident ->
  m Socket
commandLogSocketBind ident = do
  filePath <- commandLogPath ident
  socketBind filePath

pipePaneToSocket ::
  (MonadIO m, MonadFree TmuxThunk m) =>
  PaneId ->
  FilePath ->
  m ()
pipePaneToSocket paneId logPath =
  pipePane paneId cmd
  where
    cmd = "'socat STDIN UNIX-SENDTO:" ++ logPath ++ "'"

logLens :: Ident -> Lens' CommandState (Maybe CommandLog)
logLens ident = CommandState.logs . Lens.at ident

commandLogs :: MonadDeepState s CommandState m => m Logs
commandLogs =
  getsL @CommandState CommandState.logs

commandLog :: MonadDeepState s CommandState m => Ident -> m (Maybe CommandLog)
commandLog ident =
  getsL (logLens ident)

appendLog :: MonadDeepState s CommandState m => Ident -> ByteString -> m ()
appendLog ident bytes =
  modify $ Lens.over (logLens ident) append
  where
    append (Just (CommandLog prev cur)) = Just (CommandLog prev (cur <> bytes))
    append Nothing = Just (CommandLog [] bytes)
