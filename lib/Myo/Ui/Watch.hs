module Myo.Ui.Watch(
  watchPane,
) where

import Chiasma.Data.Ident (Ident)
import Conduit (runConduit, sinkNull, (.|), mapMC, mapC)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, sinkTMChan, sourceTMChan, newTMChan)
import Data.Functor (void)
import Ribosome.Control.Monad.RiboE (liftRibo)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import UnliftIO (MonadUnliftIO, atomically)
import UnliftIO.Concurrent (forkIO)

import Myo.Data.Env (Myo, MyoE)
import qualified Myo.Data.Env as Env (watcherChan)
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
  TMChan PaneOutput ->
  Myo ()
runWatcher listenChan = do
  liftIO $ putStrLn "running watcher"
  runConduit $ sourceTMChan listenChan .| mapMC handleOutput .| sinkNull

startWatcher :: MyoE e (TMChan PaneOutput)
startWatcher = do
  chan <- atomically newTMChan
  void $ liftRibo $ forkIO $ runWatcher chan
  return chan

ensureWatcher :: MyoE e (TMChan PaneOutput)
ensureWatcher = do
  current <- liftRibo $ Ribo.inspect Env.watcherChan
  maybe startWatcher return current

watchPane :: Ident -> FilePath -> MyoE e ()
watchPane ident logPath = do
  chan <- ensureWatcher
  liftRibo $ listen ident logPath chan
