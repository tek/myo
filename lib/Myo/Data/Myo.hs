module Myo.Data.Myo(
  Myo,
  Ribo,
  TVar,
  Env,
  MyoT,
  MyoTT,
  MyoE,
) where

import UnliftIO.STM (TVar)
import Ribosome.Control.Ribo (Ribo)
import Ribosome.Control.Monad.Ribo (RiboT)
import qualified Ribosome.Control.Monad.Trans.Ribo as Trans (RiboT)
import Ribosome.Control.Monad.RiboE (RiboE)
import Myo.Data.Env (Env)

type Myo a = Ribo (TVar Env) a
type MyoT = RiboT Env
type MyoTT t = Trans.RiboT t Env
type MyoE a = RiboE Env a
