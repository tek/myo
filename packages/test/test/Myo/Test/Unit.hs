module Myo.Test.Unit where

import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Hedgehog (TestT)
import Neovim.Plugin (Plugin)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Test.Embed (TestConfig(..), Vars)
import qualified Ribosome.Test.Embed as Ribosome (integrationTest)
import Ribosome.Test.Orphans ()
import qualified Ribosome.Test.Tmux as Ribosome (tmuxGuiTest, tmuxTest)
import Ribosome.Test.Tmux (RiboTesting)
import Ribosome.Test.Unit (unitTest)

import Myo.Data.Env (Env(_tempDir), Myo)
import Myo.Data.Error (Error)
import Myo.Env (bracketMyoTempDir)
import Myo.Init (initialize'')
import Myo.Plugin (plugin')
import qualified Myo.Settings as Settings (detectUi)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith, testConf)

type MyoTest a =
  TestT (Ribo Env Error) a
type MyoTestingEnv env m n =
  RiboTesting Error env m n
type MyoTesting m n =
  MyoTestingEnv (Ribosome Env) m n

test ::
  MyoTesting m n =>
  Env ->
  TestT n a ->
  TestT m a
test =
  unitTest defaultTestConfig

withTempDir ::
  MonadIO m =>
  Env ->
  (Env -> m a) ->
  m a
withTempDir env thunk =
  bracketMyoTempDir run
  where
    run tempdir =
      thunk env { _tempDir = tempdir }

testWith ::
  MyoTesting m n =>
  Env ->
  TestT n a ->
  Vars ->
  TestT m a
testWith env thunk vars =
  withTempDir env run
  where
    run env' =
      unitTest (defaultTestConfigWith vars) env' thunk

testWithDef ::
  MyoTesting m n =>
  TestT n a ->
  Vars ->
  TestT m a
testWithDef =
  testWith def

testDef ::
  MyoTesting m n =>
  TestT n a ->
  TestT m a
testDef thunk =
  testWithDef thunk def

withTmux ::
  Myo () ->
  TmuxNative ->
  Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- updateSetting Settings.detectUi True
  _ <- updateSetting tmuxSocket socket
  thunk
withTmux _ _ = fail "no socket in test tmux"

tmuxTest ::
  MyoTesting m n =>
  (TestConfig -> TestConfig) ->
  TestT n a ->
  TestT m a
tmuxTest reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxTest (testConf reconf) env thunk

tmuxGuiTest ::
  MyoTesting m n =>
  (TestConfig -> TestConfig) ->
  TestT n a ->
  TestT m a
tmuxGuiTest reconf thunk =
  withTempDir def run
  where
    run env =
      Ribosome.tmuxGuiTest (testConf reconf) env thunk

tmuxTestDef ::
  MyoTesting m n =>
  TestT n a ->
  TestT m a
tmuxTestDef =
  tmuxTest def

tmuxGuiTestDef ::
  MyoTesting m n =>
  TestT n a ->
  TestT m a
tmuxGuiTestDef =
  tmuxGuiTest def

intTest ::
  MyoTestingEnv env m n =>
  (TestConfig -> IO TestConfig) ->
  Plugin env ->
  TestT n a ->
  TestT m a
intTest reconf plug thunk = do
  conf <- liftIO (reconf defaultTestConfig)
  Ribosome.integrationTest conf plug thunk

intTestDef ::
  MonadIO m =>
  MonadFail m =>
  TestT (Ribo Env Error) a ->
  TestT m a
intTestDef thunk =
  bracketMyoTempDir run
  where
    run _ = do
      ribo <- newRibosome "myo" def
      intTest pure (plugin' ribo) (lift (disableTmux *> initialize'') *> thunk)
    disableTmux =
      updateSetting Settings.detectUi False
