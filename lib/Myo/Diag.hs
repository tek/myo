module Myo.Diag(
  myoDiag,
) where

import Data.Functor (void)
import Neovim (CommandArguments)
import Ribosome.Data.ScratchOptions (defaultScratchOptions)
import Ribosome.Scratch (showInScratch)
import Myo.Data.Myo (Myo)

diagnostics :: Myo [String]
diagnostics = return ["Diagnostics", ""]

myoDiag :: CommandArguments -> Myo ()
myoDiag _ = do
  content <- diagnostics
  void $ showInScratch content (defaultScratchOptions "myo-diagnostics")
