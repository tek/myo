module Myo.Diag where

import Control.Monad.Trans.Class (lift)
import Data.Functor (void)
import Neovim (CommandArguments)
import Ribosome.Control.Monad.Ribo (ConcNvimS, liftRibo)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Error.Report (runRiboReport)
import Ribosome.Scratch (showInScratch)

import Myo.Data.Env (Env)
import Myo.Data.Myo (Myo, MyoE)

diagnosticsData :: Myo [String]
diagnosticsData = return ["Diagnostics", ""]

diagnostics :: Myo ()
diagnostics = do
  content <- diagnosticsData
  void $ lift $ showInScratch content (defaultScratchOptions "myo-diagnostics")

myoDiag :: CommandArguments -> ConcNvimS Env ()
myoDiag _ =
  runRiboReport "diag" thunk
  where
    thunk :: MyoE () (ConcNvimS Env) ()
    thunk = liftRibo diagnostics
