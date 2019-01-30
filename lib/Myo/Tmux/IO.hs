module Myo.Tmux.IO(
  MyoTmuxProg,
  liftTmux,
  myoTmux,
  myoTmuxReport,
) where

import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)

import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Control.Monad.RiboE (RiboE(RiboE), Ribo, runRiboE)
import Ribosome.Error.Report (ReportError(..), reportError, reportErrorWith)
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Monad.Stream (TmuxProg, runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Myo.Data.Env (Env)
import Myo.Data.Myo (Myo, MyoE)
import Myo.Ui.Error (tmuxErrorReport)
import Myo.Settings (tmuxSocket)

type MyoTmuxProg = TmuxProg (Ribo Env)

liftTmux :: MyoTmuxProg a -> Myo (Either TmuxError a)
liftTmux prog = do
  socket <- settingMaybe tmuxSocket
  runTmux (TmuxNative socket) prog

myoTmux :: ExceptT e MyoTmuxProg a -> MyoE TmuxError (Either e a)
myoTmux =
  RiboE . ExceptT . liftTmux . runExceptT

myoTmuxReport :: ReportError e => ExceptT e MyoTmuxProg () -> Myo ()
myoTmuxReport ma = do
  result <- runRiboE $ myoTmux ma
  case result of
    Right (Right _) -> return ()
    Right (Left e) -> reportError e
    Left e -> reportErrorWith tmuxErrorReport e
