module Myo.Data.Myo(
  Myo,
) where

import UnliftIO.STM (TVar)
import Ribosome.Control.Ribo (Ribo)
import Myo.Data.Env (Env)

type Myo a = Ribo (TVar Env) a
