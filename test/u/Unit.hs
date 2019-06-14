module Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin.RpcHandler (RpcHandler)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationSpec)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiSpec, tmuxSpec)
import Ribosome.Test.Unit (unitSpec)

import Config (defaultTestConfig, defaultTestConfigWith, testConf)
import Myo.Data.Env (Env(_tempDir), Myo)
import Myo.Data.Error (Error)
import Myo.Env (bracketMyoTempDir)
import Myo.Init (initialize'')
import Myo.Plugin (plugin')
import qualified Myo.Settings as Settings (detectUi)
import Prelude hiding (defaultTestConfig, defaultTestConfigWith, integrationSpec)

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
withTmux _ _ = fail "no socket in test tmux"

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

intSpec ::
  NvimE e m =>
  MonadIO m =>
  ReportError e =>
  RpcHandler e env m =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  m () ->
  IO ()
intSpec reconf plug thunk = do
  conf <- reconf defaultTestConfig
  Ribosome.integrationSpec conf plug thunk

intSpecDef ::
  ReportError e =>
  RpcHandler e (Ribosome Env) (Ribo Env Error) =>
  Myo () ->
  IO ()
intSpecDef thunk =
  bracketMyoTempDir run
  where
    run _ = do
      ribo <- newRibosome "myo" def
      intSpec pure (plugin' ribo) (disableTmux *> initialize'' *> thunk)
    disableTmux =
      updateSetting Settings.detectUi False
