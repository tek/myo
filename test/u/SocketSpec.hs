{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SocketSpec (htf_thisModulesTests) where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Conduit (runConduit, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packChars)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TBMChan, newTBMChan, sinkTBMChan, sourceTBMChan, tryReadTBMChan)
import Data.Functor (void)
import Network.Socket (SockAddr(SockAddrUnix), Socket, connect)
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, toFilePath)
import Prelude hiding (tmuxSpecDef)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Config (defaultVars)
import Myo.Command.Log (commandLogPath)
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Network.Socket (socketBind, unixSocket)
import Output.Cat (addCatCommand)
import Unit (specWithDef, tmuxSpecDef)

watcher ::
  (MonadIO m, MonadBaseControl IO m) =>
  TBMChan ByteString ->
  TBMChan ByteString ->
  m ()
watcher listenChan resultChan =
  void $ fork $ runConduit $ sourceTBMChan listenChan .| sinkTBMChan resultChan

listen ::
  (MonadIO m, MonadBaseControl IO m) =>
  TBMChan ByteString ->
  Socket ->
  m ()
listen listenChan sock =
  void $ fork $ runConduit $ sourceSocket sock .| sinkTBMChan listenChan

logSocketPath :: Text -> Myo (Path Abs File)
logSocketPath =
  commandLogPath . Str

chanResult :: TBMChan a -> IO [a]
chanResult chan = do
  next <- atomically $ tryReadTBMChan chan
  case next of
    Just (Just a) -> do
      rec <- chanResult chan
      return (a : rec)
    _ -> return []

socketSpec :: Myo ()
socketSpec = do
  sockPath1 <- logSocketPath "s1"
  sockPath2 <- logSocketPath "r2"
  r1 <- socketBind sockPath1
  r2 <- socketBind sockPath2
  w1 <- unixSocket
  w2 <- unixSocket
  liftIO $ connect w1 (SockAddrUnix (toFilePath sockPath1))
  liftIO $ connect w2 (SockAddrUnix (toFilePath sockPath1))
  listenChan <- atomically $ newTBMChan 2
  resultChan <- atomically $ newTBMChan 6
  watcher listenChan resultChan
  listen listenChan r1
  send w1 "s1"
  send w1 "s1"
  send w1 "s1"
  listen listenChan r2
  send w2 "s2"
  send w2 "s2"
  send w1 "s1"
  sleep 0.1
  result <- liftIO $ chanResult resultChan
  liftIO $ assertEqual ["s1", "s1", "s1", "s2", "s2", "s1"] result
  where
    send w s = liftIO $ sendAll w $ packChars s

test_socket :: IO ()
test_socket =
  defaultVars >>= specWithDef socketSpec

monitorSpec :: Myo ()
monitorSpec = do
  initialize''
  ident <- addCatCommand "tmux"
  myoRunIdent ident
  send ["line"]
  sleep 2
  send ["line", "mine"]
  send ["line"]
  send ["line"]
  send ["line 5", "mine 12"]
  sleep 1
  where
    send =
      runTmux . sendKeys (PaneId 1)

test_monitor :: IO ()
test_monitor = do
  when debug $ tmuxSpecDef (withLog monitorSpec)
  where
    debug = False
