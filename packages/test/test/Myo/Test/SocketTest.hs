module Myo.Test.SocketTest where

import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Conduit (runConduit, (.|))
import Control.Concurrent.Lifted (fork)
import Data.ByteString.Internal (packChars)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TBMChan, newTBMChan, sinkTBMChan, sourceTBMChan, tryReadTBMChan)
import Hedgehog ((===))
import Network.Socket (SockAddr(SockAddrUnix), Socket, connect)
import Network.Socket.ByteString (sendAll)
import Path (Abs, File, Path, toFilePath)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (withLog)
import Ribosome.Tmux.Run (runTmux)

import Myo.Command.Log (commandLogPath)
import Myo.Command.Run (myoRunIdent)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Network.Socket (socketBind, unixSocket)
import Myo.Test.Config (defaultVars)
import Myo.Test.Output.Cat (addCatCommand)
import Myo.Test.Unit (MyoTest, testWithDef, tmuxTestDef)

watcher ::
  TBMChan ByteString ->
  TBMChan ByteString ->
  m ()
watcher listenChan resultChan =
  void $ fork $ runConduit $ sourceTBMChan listenChan .| sinkTBMChan resultChan

listen ::
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
      recur <- chanResult chan
      pure (a : recur)
    _ -> pure []

socketTest :: MyoTest ()
socketTest = do
  sockPath1 <- lift (logSocketPath "s1")
  sockPath2 <- lift (logSocketPath "r2")
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
  ["s1", "s1", "s1", "s2", "s2", "s1"] === result
  where
    send w s = liftIO $ sendAll w $ packChars s

test_socket :: UnitTest
test_socket =
  liftIO defaultVars >>= testWithDef socketTest

monitorTest :: MyoTest ()
monitorTest = do
  lift initialize''
  ident <- lift (addCatCommand "tmux")
  lift (myoRunIdent ident)
  send ["line"]
  sleep 2
  send ["line", "mine"]
  send ["line"]
  send ["line"]
  send ["line 5", "mine 12"]
  sleep 1
  where
    send =
      lift . runTmux . sendKeys  (PaneId 1) []

test_monitor :: UnitTest
test_monitor = do
  when debug $ tmuxTestDef (withLog monitorTest)
  where
    debug = False
