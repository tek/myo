module Myo.Init(
  initialize,
) where

import Chiasma.Data.Ident (generateIdent)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import UnliftIO.STM (TVar)

import Myo.Data.Env (Env(instanceIdent, tempDir))
import Myo.Data.Myo (Myo)
import Myo.Tmux.Runner (addTmuxRunner)

initialize' :: Myo (Ribosome (TVar Env))
initialize' = do
  addTmuxRunner
  asks' customConfig

initialize :: FilePath -> Neovim e (Ribosome (TVar Env))
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { instanceIdent = iid, tempDir = tmpdir }
  retypeNeovim (const ribo) initialize'
