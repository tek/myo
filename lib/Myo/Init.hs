module Myo.Init(
  initialize,
) where

import Chiasma.Data.Ident (generateIdent)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Monad.Ribo (runRib)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Internal.IO (retypeNeovim)
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Myo.Data.Env (Env(_instanceIdent, _tempDir))
import Myo.Data.Myo (Myo)
import Myo.Tmux.Runner (addTmuxRunner)

initialize' :: Myo (Ribosome Env)
initialize' = do
  addTmuxRunner
  lift $ asks' customConfig

initialize :: FilePath -> Neovim e (Ribosome Env)
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { _instanceIdent = iid, _tempDir = tmpdir }
  retypeNeovim (const ribo) (runRib initialize')
