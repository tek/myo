{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Neovim (Neovim)
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo(Ribo, unRibo))
import Test.Framework.AssertM (AssertM(..))
import Test.Framework.TestInterface (subAssertHTF)

instance AssertM (Neovim e) where
  genericAssertFailure__ l = liftIO . genericAssertFailure__ l
  genericSubAssert l msg ma = withRunInIO (\f -> genericSubAssert l msg (f ma))

instance AssertM (Ribo s (ConcNvimS s)) where
  genericAssertFailure__ l =
    liftIO . genericAssertFailure__ l

  genericSubAssert l msg ma =
    withRunInIO (\f -> genericSubAssert l msg (f ma))

instance AssertM (Ribo s (ExceptT e (ConcNvimS s))) where
  genericAssertFailure__ l = liftIO . genericAssertFailure__ l

  genericSubAssert l msg ma =
    Ribo $ ReaderT $ ExceptT . genericSubAssert l msg . runExceptT . runReaderT (unRibo ma)