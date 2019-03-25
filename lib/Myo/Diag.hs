module Myo.Diag where

import Data.Functor (void)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)

diagnosticsData :: Monad m => m [String]
diagnosticsData = return ["Diagnostics", ""]

myoDiag :: NvimE e m => m ()
myoDiag = do
  content <- diagnosticsData
  void $ showInScratch content (defaultScratchOptions "myo-diagnostics")
