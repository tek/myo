module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.Test.Tmux as Chiasma (tmuxSpec)
import Data.Default (def)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import Ribosome.Test.Orphans ()
import Ribosome.Test.Unit (unitSpec)
import UnliftIO (throwString)

import Config (defaultTestConfig, defaultTestConfigWith)
import Myo.Data.Env (Env(_tempDir), MyoN)
import Myo.Env (bracketMyoTempDir)
import qualified Myo.Settings as Settings (detectUi)

specConfig :: TestConfig -> Env -> MyoN () -> IO ()
specConfig =
  unitSpec

spec :: Env -> MyoN () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> MyoN () -> Vars -> IO ()
specWith env thunk vars =
  bracketMyoTempDir run
  where
    run tempdir =
      unitSpec (defaultTestConfigWith vars) env { _tempDir = tempdir } thunk

specWithDef :: MyoN () -> Vars -> IO ()
specWithDef =
  specWith def

-- FIXME need to throw when updating settings fails
withTmux :: MyoN () -> TmuxNative -> MyoN ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- setDetectUi
  _ <- setTmuxSocket
  thunk
  where
    setTmuxSocket :: MyoN ()
    setTmuxSocket = updateSetting tmuxSocket socket
    setDetectUi :: MyoN ()
    setDetectUi = updateSetting Settings.detectUi True
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpecWithDef :: MyoN () -> Vars -> IO ()
tmuxSpecWithDef thunk vars =
  Chiasma.tmuxSpec $ \api -> specWithDef (withTmux thunk api) vars
