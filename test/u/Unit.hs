module Unit where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Monad.Stream (runTmux)
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import qualified Chiasma.Test.Tmux as Chiasma (tmuxGuiSpec, tmuxSpec)
import Control.Monad.IO.Class (MonadIO)
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
import Ribosome.Config.Settings (tmuxSocket)
import Ribosome.Control.Concurrent.Wait (waitIODef)
import Ribosome.Control.Ribosome (Ribosome(Ribosome), newRibosomeTVar)
import Ribosome.Error.Report.Class (ReportError)
import Ribosome.Plugin (RpcHandler)
import Ribosome.Test.Embed (Runner, TestConfig(..), Vars, runTest)
import Ribosome.Test.Orphans ()
import Ribosome.Test.Unit (fixture, uSpec, unitSpec)
import System.FilePath ((</>))
import UnliftIO (throwString)
import UnliftIO.Async (async, cancel)
import UnliftIO.Directory (doesPathExist)
import UnliftIO.Exception (bracket)
import UnliftIO.STM (atomically, putTMVar)

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
