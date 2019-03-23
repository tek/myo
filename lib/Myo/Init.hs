{-# LANGUAGE TemplateHaskell #-}

module Myo.Init where

import Chiasma.Data.Ident (generateIdent)
import Chiasma.Data.TmuxError (TmuxError)
import Control.Monad (when)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Data.DeepPrisms (deepPrisms)
import Data.Default (Default(def))
import Data.Either (fromRight)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, liftRibo, riboE2ribo, runRib)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Myo.Data.Env (Env(_instanceIdent, _tempDir))
import Myo.Data.Myo (Myo, MyoE)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (detectDefaultUi)

data InitError =
  Setting SettingError
  |
  Tmux TmuxError
  deriving Show

deepPrisms ''InitError

initialize'' :: (MonadIO m, Nvim m, MonadRibo m, MonadMask m) => MyoE InitError m ()
initialize'' = do
  liftRibo addTmuxRunner
  detect <- setting Settings.detectUi
  when detect detectDefaultUi

initialize' :: Myo (Ribosome Env)
initialize' = do
  riboE2ribo initialize''
  lift $ asks' customConfig

-- runExceptT @SettingError $
initialize :: FilePath -> Neovim e (Ribosome Env)
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { _instanceIdent = iid, _tempDir = tmpdir }
  retypeNeovim (const ribo) (runRib initialize')
