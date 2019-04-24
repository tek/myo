module Myo.Command.Monitor where

import Chiasma.Data.Ident (Ident, sameIdent)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Monad.DeepState (MonadDeepState, getL, setL)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString.Search as ByteString (replace)
import Data.Conduit.List (unfoldM)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan, writeTMChan)
import Data.Functor (void)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import Network.Socket (Socket)
import Path (Abs, File, Path, toFilePath)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, prepend)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report (processErrorReport')
import Ribosome.System.Time (sleep)
import System.Hourglass (timeCurrent)
import System.Log (Priority(NOTICE))

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (monitorChan, pendingCommands)
import Myo.Command.Data.MonitorEvent (MonitorEvent(CommandOutput, Tick))
import Myo.Command.Data.PendingCommand (PendingCommand(PendingCommand))
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Log (appendLog)
import Myo.Command.RunningCommand (storeRunningCommand)
import qualified Myo.Log as Log (debug)
import Myo.Network.Socket (socketBind)

strictByteString :: LB.ByteString -> ByteString
strictByteString =
  ByteString.concat . LB.toChunks

replace :: Text -> Text -> ByteString -> ByteString
replace from to =
  strictByteString . ByteString.replace (encodeUtf8 from :: ByteString) (encodeUtf8 to :: ByteString)

sanitizeOutput :: ByteString -> ByteString
sanitizeOutput =
  replace "\r\n" "\n"

addPendingCommand ::
  MonadDeepState s CommandState m =>
  PendingCommand ->
  m ()
addPendingCommand =
  prepend @CommandState CommandState.pendingCommands

removePendingCommand ::
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
removePendingCommand ident =
  modifyL @CommandState CommandState.pendingCommands (filter (not . sameIdent ident))

checkPid ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  PendingCommand ->
  m ()
checkPid (PendingCommand ident findPid started) = do
  now <- liftIO timeCurrent
  done <- if now - started < Elapsed (Seconds 3) then check else return True
  when done (removePendingCommand ident)
  where
    check =
      maybe (return False) ((True <$) . storeRunningCommand ident) =<< liftIO findPid

-- TODO check running commands for being alive
handleEvent ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  MonitorEvent ->
  m ()
handleEvent (CommandOutput ident bytes) =
  appendLog ident (sanitizeOutput bytes)
handleEvent Tick = do
  pending <- getL @CommandState CommandState.pendingCommands
  traverse_ checkPid pending

listenerErrorReport :: IOException -> ErrorReport
listenerErrorReport ex =
  ErrorReport "command monitor failed" ["exception in output listener:", show ex] NOTICE

listener ::
  MonadRibo m =>
  Nvim m =>
  MonadIO m =>
  MonadBaseControl IO m =>
  Ident ->
  Socket ->
  TMChan MonitorEvent ->
  m ()
listener cmdIdent sock listenChan = do
  result <- try $ runConduit $ sourceSocket sock .| mapC (CommandOutput cmdIdent) .| sinkTMChan listenChan
  whenLeft () result (processErrorReport' "monitor" . listenerErrorReport)

listen ::
  MonadRibo m =>
  Nvim m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  Ident ->
  Path Abs File ->
  Maybe (IO (Maybe Pid)) ->
  TMChan MonitorEvent ->
  m ()
listen cmdIdent logPath findPid listenChan = do
  Log.debug $ "listening on socket at " <> toFilePath logPath
  try (socketBind logPath) >>= \case
    Right sock ->
      forkListener sock *> traverse_ enqueueCommandPid findPid
    Left (_ :: IOException) ->
      return ()
  where
    forkListener sock =
      fork $ listener cmdIdent sock listenChan
    enqueueCommandPid fp = do
      now <- liftIO timeCurrent
      addPendingCommand (PendingCommand cmdIdent fp now)

runMonitor ::
  MonadRibo m =>
  Nvim m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  TMChan MonitorEvent ->
  m ()
runMonitor listenChan =
  runConduit $ sourceTMChan listenChan .| mapMC handleEvent .| sinkNull

runClock ::
  MonadRibo m =>
  Nvim m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  TMChan MonitorEvent ->
  m ()
runClock listenChan =
  runConduit $ unfoldM unfolder () .| sinkTMChan listenChan
  where
    unfolder _ =
      sleep 0.1 $> Just (Tick, ())

startMonitor ::
  MonadRibo m =>
  Nvim m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  m (TMChan MonitorEvent)
startMonitor = do
  chan <- atomically newTMChan
  void $ fork $ runMonitor chan
  void $ fork $ runClock chan
  setL @CommandState CommandState.monitorChan (Just chan)
  return chan

ensureMonitor ::
  MonadRibo m =>
  Nvim m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  m (TMChan MonitorEvent)
ensureMonitor = do
  current <- getL @CommandState CommandState.monitorChan
  maybe startMonitor return current

monitorCommand ::
  MonadRibo m =>
  Nvim m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Path Abs File ->
  Maybe (IO (Maybe Pid)) ->
  m ()
monitorCommand cmdIdent logPath findPid =
  listen cmdIdent logPath findPid =<< ensureMonitor
