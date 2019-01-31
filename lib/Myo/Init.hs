module Myo.Init(
  initialize,
) where

import Data.Default (Default(def))
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import Control.Monad.IO.Class (liftIO)
import UnliftIO.STM (TVar)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Chiasma.Data.Ident (generateIdent)
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Internal.IO (retypeNeovim)
import Myo.Data.Myo (Myo)
import Myo.Data.Env (Env(instanceIdent, tempDir))

initialize' :: Myo (Ribosome (TVar Env))
initialize' =
  asks' customConfig

initialize :: FilePath -> Neovim e (Ribosome (TVar Env))
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { instanceIdent = iid, tempDir = tmpdir }
  retypeNeovim (const ribo) initialize'
