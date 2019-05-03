module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Data.Default (def)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)
import UnliftIO (throwString)

import Config (defaultTestConfig, defaultTestConfigWith, testConf)
import Myo.Data.Env (Env(_tempDir), Myo)
import Myo.Env (bracketMyoTempDir)
import qualified Myo.Settings as Settings (detectUi)

specConfig :: TestConfig -> Env -> Myo () -> IO ()
specConfig =
  unitSpec

spec :: Env -> Myo () -> IO ()
spec =
  specConfig defaultTestConfig

withTempDir :: Env -> (Env -> IO ()) -> IO ()
withTempDir env thunk =
  bracketMyoTempDir run
  where
    run tempdir =
      thunk env { _tempDir = tempdir }

specWith :: Env -> Myo () -> Vars -> IO ()
specWith env thunk vars =
  withTempDir env run
  where
    run env' =
      unitSpec (defaultTestConfigWith vars) env' thunk

specWithDef :: Myo () -> Vars -> IO ()
specWithDef =
  specWith def

specDef :: Myo () -> IO ()
specDef thunk =
  specWithDef thunk def

withTmux :: Myo () -> TmuxNative -> Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting Settings.detectUi True
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpec :: (TestConfig -> TestConfig) -> Myo () -> IO ()
tmuxSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxSpec (testConf reconf) env thunk

tmuxGuiSpec :: (TestConfig -> TestConfig) -> Myo () -> IO ()
tmuxGuiSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxGuiSpec (testConf reconf) env thunk

tmuxSpecDef :: Myo () -> IO ()
tmuxSpecDef =
  tmuxSpec def

tmuxGuiSpecDef :: Myo () -> IO ()
tmuxGuiSpecDef =
  tmuxGuiSpec def
