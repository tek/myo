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
import qualified Control.Lens as Lens (at, view)
import Control.Monad.Base (MonadBase)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState, gets, modify)
import Control.Monad.Free (MonadFree)
import Network.Socket (Socket)
import System.FilePath ((</>))

import qualified Myo.Command.Data.CommandState as CommandState (logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (command, tempDir)
import Myo.Network.Socket (socketBind)

logLens :: Ident -> Lens' Env (Maybe FilePath)
logLens ident = Env.command . CommandState.logs . Lens.at ident

logByIdent ::
  (MonadDeepError e RunError m, MonadDeepState s Env m) =>
  Ident ->
  m (Maybe FilePath)
logByIdent ident =
  gets $ Lens.view $ logLens ident

logTempDir ::
  (MonadDeepState s Env m) =>
  m FilePath
logTempDir =
  gets g
  where
    g :: Env -> String
    g = Lens.view Env.tempDir

insertLog ::
  (MonadDeepError e RunError m, MonadDeepState s Env m) =>
  Ident ->
  m FilePath
insertLog ident = do
  base <- logTempDir
  let logPath = base </> "pane-" ++ identString ident
  modify $ logLens ident ?~ logPath
  return logPath

commandLog ::
  (MonadDeepError e RunError m, MonadDeepState s Env m) =>
  Ident ->
  m FilePath
commandLog ident = do
  existing <- logByIdent ident
  maybe (insertLog ident) return existing

commandLogSocketBind ::
  (MonadDeepError e RunError m, MonadDeepState s Env m, MonadBase IO m) =>
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
