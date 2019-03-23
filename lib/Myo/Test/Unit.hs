module Myo.Test.Unit where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (tmuxGuiSpec, tmuxSpec)
import Control.Monad.Trans.Except (runExceptT)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Neovim (Neovim)
import qualified Neovim.Context.Internal as Internal (
  Config,
  Neovim(Neovim),
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
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, riboE2ribo, runRib)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTVar)
import Ribosome.Data.Time (sleep)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Test.Embed (Runner, TestConfig(..), Vars, runTest)
import Ribosome.Test.Unit (tempDir, uSpec, unitSpecR)
import System.FilePath ((</>))
import UnliftIO (throwString)
import UnliftIO.Async (async, cancel, race)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception (bracket, tryAny)
import UnliftIO.STM (atomically, putTMVar)

import Myo.Data.Env (Env(_tempDir))
import Myo.Data.Myo (Myo, MyoE)
import Myo.Env (bracketMyoTempDir)
import Myo.Settings (tmuxSocket)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Myo.Ui.Default (setupDefaultTestUi)

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

-- FIXME need to throw when updating settings fails
withTmux :: Myo () -> TmuxNative -> Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  _ <- riboE2ribo setDetectUi
  _ <- riboE2ribo setTmuxSocket
  setupDefaultTestUi
  thunk
  where
    setTmuxSocket :: MyoE RpcError (ConcNvimS Env) ()
    setTmuxSocket = updateSetting tmuxSocket socket
    setDetectUi :: MyoE RpcError (ConcNvimS Env) ()
    setDetectUi = updateSetting Settings.detectUi False
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpecWithDef :: Myo () -> Vars -> IO ()
tmuxSpecWithDef thunk vars =
  tmuxSpec $ \api -> specWithDef (withTmux thunk api) vars

tmuxGuiSpecWithDef :: Myo () -> Vars -> IO ()
tmuxGuiSpecWithDef thunk vars =
  tmuxGuiSpec $ \api -> specWithDef (withTmux thunk api) vars

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

runExternalNvim :: TestConfig -> s -> Neovim s () -> FilePath -> IO ()
runExternalNvim conf ribo thunk socket = do
  nvimConf <- Internal.newConfig (pure Nothing) newRPCConfig
  let testCfg = Internal.retypeConfig ribo nvimConf
  bracket (startHandlers socket conf nvimConf) id (runTest conf testCfg thunk)

externalNvimCmdline :: FilePath -> String
externalNvimCmdline socket =
  "nvim --listen " ++ socket ++ " -n -u NONE -i NONE"

runExternal :: TmuxNative -> FilePath -> TestConfig -> s -> Neovim s () -> IO ()
runExternal api temp conf ribo thunk = do
  runExceptT @TmuxError $ runTmux api $ sendKeys (PaneId 0) [externalNvimCmdline socket]
  _ <- waitIODef (pure socket) doesPathExist
  runExternalNvim conf ribo thunk socket
  where
    socket = temp </> "nvim-socket"

unsafeExternalSpec :: TmuxNative -> FilePath -> Runner s -> TestConfig -> s -> Neovim s () -> IO ()
unsafeExternalSpec api temp runner conf s spec =
  runExternal api temp conf s $ runner conf spec

unsafeExternalSpecR ::
  TmuxNative ->
  FilePath ->
  Runner (Ribosome s) ->
  TestConfig ->
  s ->
  Ribo s (ConcNvimS s) () ->
  IO ()
unsafeExternalSpecR api temp runner conf s spec = do
  tv <- newRibosomeTVar s
  let ribo = Ribosome (tcPluginName conf) tv
  unsafeExternalSpec api temp runner conf ribo (runRib spec)

externalSpec :: TmuxNative -> Env -> Myo () -> Vars -> IO ()
externalSpec api env thunk vars =
  bracketMyoTempDir run
  where
    run tempdir =
      unsafeExternalSpecR api tempdir uSpec (defaultTestConfigWith vars) env { _tempDir = tempdir } thunk

tmuxExternalSpec :: Myo () -> Vars -> IO ()
tmuxExternalSpec thunk vars =
  tmuxGuiSpec run
  where
    run api = externalSpec api def (withTmux thunk api) vars
