module Myo.Test.Unit(
  spec,
  specWith,
  specWithDef,
  specConfig,
  tmuxSpecWithDef,
  tmuxGuiSpecWithDef,
) where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (tmuxSpec, tmuxGuiSpec)
import Data.Default (def)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Embed (Vars, TestConfig)
import Ribosome.Test.Unit (unitSpec)
import UnliftIO (throwString)
import UnliftIO.STM (newTVarIO)

import Myo.Data.Env (Env(tempDir))
import Myo.Data.Myo (Myo)
import Myo.Env (bracketMyoTempDir)
import Myo.Settings (tmuxSocket)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Myo.Ui.Default (setupDefaultUi)

specConfig :: TestConfig -> Env -> Myo () -> IO ()
specConfig conf e s = do
  t <- newTVarIO e
  unitSpec conf t s

spec :: Env -> Myo () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Myo () -> Vars -> IO ()
specWith env thunk vars =
  bracketMyoTempDir run
  where
    run tempdir = do
      t <- newTVarIO env { tempDir = tempdir }
      unitSpec (defaultTestConfigWith vars) t thunk

specWithDef :: Myo () -> Vars -> IO ()
specWithDef =
  specWith def

withTmux :: Myo () -> TmuxNative -> Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  updateSetting tmuxSocket socket
  setupDefaultUi
  thunk
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpecWithDef :: Myo () -> Vars -> IO ()
tmuxSpecWithDef thunk vars =
  tmuxSpec $ \api -> specWithDef (withTmux thunk api) vars

tmuxGuiSpecWithDef :: Myo () -> Vars -> IO ()
tmuxGuiSpecWithDef thunk vars =
  tmuxGuiSpec $ \api -> specWithDef (withTmux thunk api) vars
