module Test where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Neovim (Neovim)
import Test.Framework.AssertM (AssertM(..))
import Test.Framework.TestInterface (subAssertHTF)

instance AssertM (Neovim e) where
  genericAssertFailure__ l = liftIO . genericAssertFailure__ l
  genericSubAssert l msg ma = withRunInIO (\f -> genericSubAssert l msg (f ma))
