module Myo.Ui.Watch(
  watchPane,
) where

import Chiasma.Data.Ident (Ident)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (gets)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan)
import Data.Functor (void)
import Ribosome.Control.Monad.Ribo (Ribo, liftRibo)
import UnliftIO (MonadUnliftIO, atomically)
import UnliftIO.Concurrent (forkIO)

import Myo.Data.Env (Env, MyoE)
import qualified Myo.Data.Env as Env (_watcherChan)
import Myo.Network.Socket (socketBind)
import Myo.Ui.Data.PaneOutput (PaneOutput(PaneOutput))

handleOutput :: MonadIO m => PaneOutput -> m ()
handleOutput o = do
  liftIO $ print o
  return ()

listen ::
  (MonadIO m, MonadUnliftIO m) =>
  Ident ->
  FilePath ->
  TMChan PaneOutput ->
  m ()
listen ident logPath listenChan = do
  liftIO $ putStrLn $ "listening on socket at " ++ logPath
  sock <- socketBind logPath
  void $ forkIO $ runConduit $ sourceSocket sock .| mapC (PaneOutput ident) .| sinkTMChan listenChan

runWatcher ::
  MonadIO m =>
  TMChan PaneOutput ->
  Ribo Env m ()
runWatcher listenChan = do
  liftIO $ putStrLn "running watcher"
  runConduit $ sourceTMChan listenChan .| mapMC handleOutput .| sinkNull

startWatcher :: MonadUnliftIO m => MyoE e m (TMChan PaneOutput)
startWatcher = do
  chan <- atomically newTMChan
  void $ liftRibo $ forkIO $ runWatcher chan
  return chan

ensureWatcher :: MonadUnliftIO m => MyoE e m (TMChan PaneOutput)
ensureWatcher = do
  current <- liftRibo $ gets Env._watcherChan
  maybe startWatcher return current

watchPane :: MonadUnliftIO m => Ident -> FilePath -> MyoE e m ()
watchPane ident logPath = do
  chan <- ensureWatcher
  liftRibo $ listen ident logPath chan
