module Myo.Tmux.IO where

import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Monad.Stream (TmuxProg, runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Control.Monad ((=<<))
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Class (lift)
import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Control.Monad.Ribo (ConcNvimS, MonadRibo, Nvim, Ribo(Ribo))

import Myo.Data.Env (Env)
import Myo.Data.Myo (MyoE)
import Myo.Orphans ()
import Myo.Settings (tmuxSocket)

type MyoTmuxProg m = TmuxProg (Ribo Env m)

runTmuxN ::
  (MonadRibo m, MonadDeepError e TmuxError m, MonadUnliftIO m, Nvim m, MonadThrow m) =>
  TmuxProg m a ->
  m a
runTmuxN prog = do
  socket <- settingMaybe tmuxSocket
  hoistEither =<< runTmux (TmuxNative socket) prog

runTmuxE ::
  (MonadRibo m, MonadUnliftIO m, Nvim m, MonadThrow m) =>
  TmuxProg m a ->
  m (Either TmuxError a)
runTmuxE prog = do
  socket <- settingMaybe tmuxSocket
  runTmux (TmuxNative socket) prog

myoTmux ::
  (MonadRibo m, MonadUnliftIO m, Nvim m, MonadThrow m) =>
  TmuxProg m a ->
  MyoE TmuxError m a
myoTmux prog = do
  socket <- settingMaybe tmuxSocket
  result <- Ribo . lift . lift $ runTmux (TmuxNative socket) prog
  Ribo . lift . liftEither $ result

-- myoTmuxReport :: ReportError e => String -> TmuxProg m () -> Myo ()
-- myoTmuxReport componentName ma = do
--   result <- runRiboE $ myoTmux ma
--   case result of
--     Right (Right _) -> return ()
--     Right (Left e) -> reportError componentName e
--     Left e -> reportErrorWith componentName tmuxErrorReport e

class RunTmux m where
  runMyoTmux :: TmuxProg m b -> m (Either TmuxError b)

instance RunTmux (Ribo Env (ConcNvimS Env)) where
  runMyoTmux = runTmuxE
