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
import Myo.Data.Env (Env(_tempDir), MyoN)
import Myo.Env (bracketMyoTempDir)
import qualified Myo.Settings as Settings (detectUi)

specConfig :: TestConfig -> Env -> MyoN () -> IO ()
specConfig =
  unitSpec

spec :: Env -> MyoN () -> IO ()
spec =
  specConfig defaultTestConfig

withTempDir :: Env -> (Env -> IO ()) -> IO ()
withTempDir env thunk =
  bracketMyoTempDir run
  where
    run tempdir =
      thunk env { _tempDir = tempdir }

specWith :: Env -> MyoN () -> Vars -> IO ()
specWith env thunk vars =
  withTempDir env run
  where
    run env' =
      unitSpec (defaultTestConfigWith vars) env' thunk

specWithDef :: MyoN () -> Vars -> IO ()
specWithDef =
  specWith def

withTmux :: MyoN () -> TmuxNative -> MyoN ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting Settings.detectUi True
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpec :: (TestConfig -> TestConfig) -> MyoN () -> IO ()
tmuxSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxSpec (testConf reconf) env thunk

tmuxGuiSpec :: (TestConfig -> TestConfig) -> MyoN () -> IO ()
tmuxGuiSpec reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxGuiSpec (testConf reconf) env thunk

tmuxSpecDef :: MyoN () -> IO ()
tmuxSpecDef =
  tmuxSpec def

tmuxGuiSpecDef :: MyoN () -> IO ()
tmuxGuiSpecDef =
  tmuxGuiSpec def
