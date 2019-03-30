{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxThunk (TmuxThunk)
import Control.Monad.Catch (MonadMask)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.Free.Class (MonadFree)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Neovim (Neovim)
import Ribosome.Control.Monad.Ribo (ConcNvimS, MonadRibo, Nvim, Ribo(Ribo, unRibo))
import Test.Framework
import Test.Framework.AssertM (AssertM(..))
import Test.Framework.TestInterface (subAssertHTF)

import qualified Myo.Test.Unit as Myo (screenshot)
import Myo.Tmux.IO (runTmux)

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

screenshot ::
  AssertM m =>
  MonadIO m =>
  MonadRibo m =>
  MonadDeepError e TmuxError m =>
  MonadMask m =>
  Nvim m =>
  String ->
  Bool ->
  Int ->
  m ()
screenshot name record pane =
  (runTmux $ Myo.screenshot name record pane) >>= check
  where
    check (Just (current, existing)) =
      gassertEqual existing current
    check Nothing =
      return ()
