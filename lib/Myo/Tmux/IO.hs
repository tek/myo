module Myo.Tmux.IO where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Monad.Stream (TmuxProg)
import qualified Chiasma.Monad.Stream as Chiasma (runTmux, runTmuxE)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Control.Monad ((=<<))
import Control.Monad.Catch (MonadMask)
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.DeepPrisms (DeepPrisms)
import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Control.Monad.Ribo (ConcNvimS, MonadRibo, Nvim, Ribo(Ribo), RiboE)
import Ribosome.Orphans ()

import Myo.Data.Env (Env)
import Myo.Data.Myo (MyoE)
import Myo.Orphans ()
import Myo.Settings (tmuxSocket)

type MyoTmuxProg m = TmuxProg (Ribo Env m)

runTmux ::
  (MonadIO m, MonadRibo m, MonadDeepError e TmuxError m, MonadMask m, Nvim m) =>
  TmuxProg m a ->
  m a
runTmux prog = do
  socket <- settingMaybe tmuxSocket
  Chiasma.runTmux (TmuxNative socket) prog

runTmuxE ::
  (MonadIO m, MonadRibo m, MonadMask m, Nvim m) =>
  TmuxProg (ExceptT TmuxError m) a ->
  m (Either TmuxError a)
runTmuxE =
  runExceptT . runTmux

-- myoTmuxReport :: ReportError e => String -> TmuxProg m () -> Myo ()
-- myoTmuxReport componentName ma = do
--   result <- runRiboE $ myoTmux ma
--   case result of
--     Right (Right _) -> return ()
--     Right (Left e) -> reportError componentName e
--     Left e -> reportErrorWith componentName tmuxErrorReport e

class RunTmux m where
  runMyoTmux :: TmuxProg m b -> m b

instance (MonadIO m, MonadRibo m, DeepPrisms e TmuxError, MonadMask m, Nvim m) =>
  RunTmux (RiboE Env e m) where
    runMyoTmux = runTmux
