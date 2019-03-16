module Myo.Test.Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (tmuxGuiSpec, tmuxSpec)
import Data.Default (def)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Control.Monad.Ribo (ConcNvimS, riboE2ribo)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Test.Embed (TestConfig, Vars)
import Ribosome.Test.Unit (unitSpecR)
import UnliftIO (throwString)

import Myo.Data.Env (Env(_tempDir))
import Myo.Data.Myo (Myo, MyoE)
import Myo.Env (bracketMyoTempDir)
import Myo.Settings (tmuxSocket)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Myo.Ui.Default (setupDefaultUi)

specConfig :: TestConfig -> Env -> Myo () -> IO ()
specConfig =
  unitSpecR

spec :: Env -> Myo () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Myo () -> Vars -> IO ()
specWith env thunk vars =
  bracketMyoTempDir run
  where
    run tempdir =
      unitSpecR (defaultTestConfigWith vars) env { _tempDir = tempdir } thunk

specWithDef :: Myo () -> Vars -> IO ()
specWithDef =
  specWith def

-- FIXME need to throw when setting socket fails
withTmux :: Myo () -> TmuxNative -> Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- riboE2ribo setTmuxSocket
  setupDefaultUi
  thunk
  where
    setTmuxSocket :: MyoE RpcError (ConcNvimS Env) ()
    setTmuxSocket = updateSetting tmuxSocket socket
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpecWithDef :: Myo () -> Vars -> IO ()
tmuxSpecWithDef thunk vars =
  tmuxSpec $ \api -> specWithDef (withTmux thunk api) vars

tmuxGuiSpecWithDef :: Myo () -> Vars -> IO ()
tmuxGuiSpecWithDef thunk vars =
  tmuxGuiSpec $ \api -> specWithDef (withTmux thunk api) vars
