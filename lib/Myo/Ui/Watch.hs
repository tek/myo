module Myo.Ui.Watch(
  watchPane,
) where

import Chiasma.Data.Ident (Ident)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Concurrent.Lifted (fork)
import qualified Control.Lens as Lens (set, view)
import Control.Monad.DeepState (MonadDeepState, gets, modify, setL)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan)
import Data.Functor (void)
import UnliftIO (atomically)

import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (watcherChan)
import Myo.Network.Socket (socketBind)
import Myo.Ui.Data.PaneOutput (PaneOutput(PaneOutput))

handleOutput :: MonadIO m => PaneOutput -> m ()
handleOutput o = do
  liftIO $ print o
  return ()

listen ::
  (MonadIO m, MonadBaseControl IO m) =>
  Ident ->
  FilePath ->
  TMChan PaneOutput ->
  m ()
listen ident logPath listenChan = do
  -- liftBase $ putStrLn $ "listening on socket at " ++ logPath
  sock <- socketBind logPath
  void $ fork $ runConduit $ sourceSocket sock .| mapC (PaneOutput ident) .| sinkTMChan listenChan

runWatcher ::
  MonadIO m =>
  TMChan PaneOutput ->
  m ()
runWatcher listenChan =
  runConduit $ sourceTMChan listenChan .| mapMC handleOutput .| sinkNull

startWatcher :: ∀ s m. (MonadBaseControl IO m, MonadIO m, MonadDeepState s Env m) => m (TMChan PaneOutput)
startWatcher = do
  chan <- atomically newTMChan
  void $ fork $ runWatcher chan
  modify @s @Env $ Lens.set Env.watcherChan (Just chan)
  setL Env.watcherChan (Just chan)
  return chan

ensureWatcher :: ∀ s m. (MonadBaseControl IO m, MonadIO m, MonadDeepState s Env m) => m (TMChan PaneOutput)
ensureWatcher = do
  current <- gets @s @Env (Lens.view Env.watcherChan)
  maybe startWatcher return current

watchPane :: (MonadBaseControl IO m, MonadIO m, MonadDeepState s Env m) => Ident -> FilePath -> m ()
watchPane ident logPath = do
  chan <- ensureWatcher
  listen ident logPath chan
