module Myo.Command.Log(
  commandLog,
) where

import Chiasma.Data.Ident (Ident, identString)
import Control.Lens (Lens', (?~))
import qualified Control.Lens as Lens (view, at)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState, gets, modify)
import System.FilePath ((</>))

import qualified Myo.Command.Data.CommandState as CommandState (_logs)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (_tempDir, _command)

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
