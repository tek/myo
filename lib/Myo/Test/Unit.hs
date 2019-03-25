module Myo.Test.Unit where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.Test.Tmux as Chiasma (tmuxGuiSpec, tmuxSpec)
import Control.Monad.Trans.Except (runExceptT)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Functor (void)
import qualified Neovim.Context.Internal as Internal (
  Config,
  globalFunctionMap,
  mkFunctionMap,
  newConfig,
  pluginSettings,
  retypeConfig,
  )
import Neovim.RPC.Common (RPCConfig, SocketType(UnixSocket), createHandle, newRPCConfig)
import Neovim.RPC.EventHandler (runEventHandler)
import Neovim.RPC.SocketReader (runSocketReader)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Control.Concurrent.Wait (waitIODef)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTVar)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin (RpcHandler)
import Ribosome.Test.Embed (Runner, TestConfig(..), Vars, runTest)
import Ribosome.Test.Unit (uSpec, unitSpec)
import System.FilePath ((</>))
import UnliftIO (throwString)
import UnliftIO.Async (async, cancel)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (atomically, putTMVar)

import Myo.Data.Env (Env(_tempDir), MyoN)
import Myo.Env (bracketMyoTempDir)
import Myo.Settings (tmuxSocket)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)

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

startHandlers :: FilePath -> TestConfig -> Internal.Config RPCConfig -> IO (IO ())
startHandlers socket TestConfig{..} nvimConf = do
  handle <- createHandle (UnixSocket socket)
  socketReader <- run runSocketReader handle
  eventHandler <- run runEventHandler handle
  atomically $ putTMVar (Internal.globalFunctionMap nvimConf) (Internal.mkFunctionMap [])
  let stopEventHandlers = traverse_ cancel [socketReader, eventHandler]
  return stopEventHandlers
  where
    run runner hand = async . void $ runner hand emptyConf
    emptyConf = nvimConf { Internal.pluginSettings = Nothing }

runTmuxNvim ::
  (RpcHandler e env m, ReportError e) =>
  TestConfig ->
  env ->
  m () ->
  FilePath ->
  IO ()
runTmuxNvim conf ribo specThunk socket = do
  nvimConf <- Internal.newConfig (pure Nothing) newRPCConfig
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startHandlers socket conf nvimConf) id (runTest conf testCfg specThunk)

externalNvimCmdline :: FilePath -> String
externalNvimCmdline socket =
  "nvim --listen " ++ socket ++ " -n -u NONE -i NONE"

runGui ::
  (RpcHandler e env m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  TestConfig ->
  env ->
  m () ->
  IO ()
runGui api temp conf ribo specThunk = do
  void $ runExceptT @TmuxError $ runTmux api $ sendKeys (PaneId 0) [externalNvimCmdline socket]
  _ <- waitIODef (pure socket) doesPathExist
  runTmuxNvim conf ribo specThunk socket
  where
    socket = temp </> "nvim-socket"

unsafeGuiSpec ::
  (RpcHandler e env m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeGuiSpec api temp runner conf s specThunk =
  runGui api temp conf s $ runner conf specThunk

unsafeGuiSpecR ::
  (RpcHandler e (Ribosome env) m, ReportError e) =>
  TmuxNative ->
  FilePath ->
  Runner m ->
  TestConfig ->
  env ->
  m () ->
  IO ()
unsafeGuiSpecR api temp runner conf s specThunk = do
  tv <- newRibosomeTVar s
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeGuiSpec api temp runner conf ribo specThunk

guiSpec :: TmuxNative -> Env -> MyoN () -> Vars -> IO ()
guiSpec api env specThunk vars =
  bracketMyoTempDir run
  where
    run tempdir =
      unsafeGuiSpecR api tempdir uSpec (defaultTestConfigWith vars) env { _tempDir = tempdir } specThunk

tmuxGuiSpec :: MyoN () -> Vars -> IO ()
tmuxGuiSpec specThunk vars =
  Chiasma.tmuxGuiSpec run
  where
    run api = guiSpec api def (withTmux specThunk api) vars
