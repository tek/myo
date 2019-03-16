{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SocketSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Test.Tmux (sleep)
import Conduit (runConduit, (.|))
import Config (vars)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (packChars)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TBMChan, newTBMChan, sinkTBMChan, sourceTBMChan, tryReadTBMChan)
import Data.Functor (void)
import Network.Socket (SockAddr(SockAddrUnix), Socket, connect)
import Network.Socket.ByteString (sendAll)
import Ribosome.Control.Monad.Ribo (ConcNvimS, riboE2ribo)
import Test.Framework
import UnliftIO (atomically)
import UnliftIO.Concurrent (forkIO)

import Myo.Command.Data.RunError (RunError)
import Myo.Command.Log (commandLog)
import Myo.Data.Myo (Env, Myo, MyoE)
import Myo.Network.Socket (socketBind, unixSocket)
import Myo.Test.Unit (specWithDef)

watcher ::
  (MonadIO m, MonadUnliftIO m) =>
  TBMChan ByteString ->
  TBMChan ByteString ->
  m ()
watcher listenChan resultChan =
  void $ forkIO $ runConduit $ sourceTBMChan listenChan .| sinkTBMChan resultChan

listen ::
  (MonadIO m, MonadUnliftIO m) =>
  TBMChan ByteString ->
  Socket ->
  m ()
listen listenChan sock =
  void $ forkIO $ runConduit $ sourceSocket sock .| sinkTBMChan listenChan

logSocketPath :: String -> Myo FilePath
logSocketPath name = do
  fp <- riboE2ribo lg
  liftIO $ either (const $ return "") return fp
  where
    lg :: MyoE RunError (ConcNvimS Env) FilePath
    lg = commandLog (Str name)

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
  liftIO $ connect w1 (SockAddrUnix sockPath1)
  liftIO $ connect w2 (SockAddrUnix sockPath1)
  listenChan <- atomically $ newTBMChan 2
  resultChan <- atomically $ newTBMChan 6
  watcher listenChan resultChan
  listen listenChan r1
  liftIO $ sendAll w1 $ packChars "s1"
  liftIO $ sendAll w1 $ packChars "s1"
  liftIO $ sendAll w1 $ packChars "s1"
  listen listenChan r2
  liftIO $ sendAll w2 $ packChars "s2"
  liftIO $ sendAll w2 $ packChars "s2"
  liftIO $ sendAll w1 $ packChars "s1"
  sleep 0.1
  result <- liftIO $ chanResult resultChan
  liftIO $ assertEqual ["s1", "s1", "s1", "s2", "s2", "s1"] result

test_socket :: IO ()
test_socket =
  vars >>= specWithDef socketSpec
