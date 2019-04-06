{-# LANGUAGE TemplateHaskell #-}

module Myo.Init where

import Chiasma.Data.Ident (generateIdent)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.DeepPrisms (deepPrisms)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (asks', Config(customConfig))
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (NvimE, riboE2ribo, runRib)
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report (reportError')
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Parse (addHandler)
import Myo.Data.Env (Env(_instanceIdent, _tempDir), Myo, MyoN)
import Myo.Orphans ()
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (detectDefaultUi)
import Myo.Ui.Error (tmuxErrorReport, viewsErrorReport)

data InitError =
  Setting SettingError
  |
  Tmux TmuxError
  |
  Views ViewsError
  |
  Rpc RpcError
  deriving Show

deepPrisms ''InitError

instance ReportError InitError where
  errorReport (Setting e) = errorReport e
  errorReport (Tmux e) = tmuxErrorReport e
  errorReport (Views e) = viewsErrorReport e
  errorReport (Rpc e) = errorReport e

initialize'' :: MyoN ()
initialize'' = do
  addTmuxRunner
  addHandler (CommandLanguage "haskell") (OutputHandler haskellOutputParser)
  detect <- setting Settings.detectUi
  when detect detectDefaultUi

initialize' :: Myo (Ribosome Env)
initialize' = do
  result <- riboE2ribo initialize''
  reportError' "init" result
  lift $ asks' customConfig

initialize :: FilePath -> Neovim e (Ribosome Env)
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { _instanceIdent = iid, _tempDir = tmpdir }
  retypeNeovim (const ribo) (runRib initialize')

myoPoll :: NvimE e m => m Bool
myoPoll =
  return True
