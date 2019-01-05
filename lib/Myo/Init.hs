module Myo.Init(
  initialize,
) where

import Data.Default.Class (Default(def))
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(ERROR))
import Control.Monad.IO.Class (liftIO)
import UnliftIO.STM (TVar)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Internal.IO (retypeNeovim)
import Myo.Data.Myo (Myo)
import Myo.Data.Env (Env)

initialize' :: Myo (Ribosome (TVar Env))
initialize' =
  asks' customConfig

initialize :: Neovim e (Ribosome (TVar Env))
initialize = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  ribo <- newRibosome "myo" def
  retypeNeovim (const ribo) initialize'
