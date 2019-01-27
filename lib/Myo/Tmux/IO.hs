module Myo.Tmux.IO(
  liftTmux,
) where

import UnliftIO.STM (TVar)
import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Control.Ribo (Ribo)
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Monad.Stream (TmuxProg, runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Myo.Data.Env (Env)
import Myo.Data.Myo (Myo)
import Myo.Settings (tmuxSocket)

type MyoTmuxProg = TmuxProg (Ribo (TVar Env))

liftTmux :: MyoTmuxProg a -> Myo (Either TmuxError a)
liftTmux prog = do
  socket <- settingMaybe tmuxSocket
  runTmux (TmuxNative socket) prog
