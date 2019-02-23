module Myo.Command.Log(
  commandLog,
  commandLogSocketBind,
  pipePaneToSocket,
) where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Control.Lens (Lens', (?~))
import qualified Control.Lens as Lens (view, at)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Free (MonadFree)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState, gets, modify)
import Network.Socket (Socket)
import System.FilePath ((</>))

import qualified Myo.Command.Data.CommandState as CommandState (_logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (_tempDir, _command)
import Myo.Network.Socket (socketBind)

logLens :: Ident -> Lens' Env (Maybe FilePath)
logLens ident = Env._command . CommandState._logs . Lens.at ident

logByIdent ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m (Maybe FilePath)
logByIdent ident =
  gets $ Lens.view $ logLens ident

logTempDir ::
  (MonadState Env m) =>
  m FilePath
logTempDir =
  gets $ Lens.view Env._tempDir

insertLog ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m FilePath
insertLog ident = do
  base <- logTempDir
  let logPath = base </> "pane-" ++ identString ident
  modify $ logLens ident ?~ logPath
  return logPath

commandLog ::
  (MonadError RunError m, MonadState Env m) =>
  Ident ->
  m FilePath
commandLog ident = do
  existing <- logByIdent ident
  maybe (insertLog ident) return existing

commandLogSocketBind ::
  (MonadError RunError m, MonadState Env m, MonadIO m) =>
  Ident ->
  m Socket
commandLogSocketBind ident = do
  filePath <- commandLog ident
  socketBind filePath

pipePaneToSocket ::
  MonadFree TmuxThunk m =>
  PaneId ->
  FilePath ->
  m ()
pipePaneToSocket paneId logPath =
  pipePane paneId cmd
  where
    cmd = "socat UNIX-LISTEN:" ++ logPath ++ " -"
