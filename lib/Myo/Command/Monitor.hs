module Myo.Command.Monitor where

import Chiasma.Data.Ident (Ident, identText)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Exception (IOException)
import Control.Exception.Lifted (try, throw)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString.Search as ByteString (replace)
import Data.Conduit.List (unfoldM)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import Network.Socket (Socket)
import Path (Abs, File, Path, toFilePath)
import Prelude hiding (state)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Exception (catchAny, tryAny)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report (processErrorReport')
import Ribosome.System.Time (sleep)
import System.Hourglass (timeCurrent)
import System.Log (Priority(DEBUG))

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (executing, monitorChan)
import Myo.Command.Data.Execution (Execution(Execution), ExecutionMonitor(ExecutionMonitor), ExecutionState(..))
import Myo.Command.Data.MonitorEvent (MonitorEvent(CommandOutput, Tick))
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Execution (killExecution, modifyExecutionState, setExecutionState, storeOutputSocket)
import Myo.Command.Log (appendLog)
import Myo.Network.Socket (socketBind)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.System.Proc (processExists)

strictByteString :: LB.ByteString -> ByteString
strictByteString =
  ByteString.concat . LB.toChunks

replace :: Text -> Text -> ByteString -> ByteString
replace from to =
  strictByteString . ByteString.replace (encodeUtf8 from :: ByteString) (encodeUtf8 to :: ByteString)

sanitizeOutput :: ByteString -> ByteString
sanitizeOutput =
  replace "\r\n" "\n"

checkTracked ::
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Pid ->
  m ()
checkTracked ident pid = do
  exists <- processExists pid
  unless exists kill
  where
    kill = do
      logDebug $ "tracked command `" <> identText ident <> "` has no process anymore"
      void . tryAny $ killExecution ident

promoteExecution ::
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m ()
promoteExecution ident =
  modifyExecutionState ident trans
  where
    trans Pending = do
      logDebug $ "pid detection for `" <> identText ident <> "` timed out"
      return Unknown
    trans (Starting pid) = do
      logDebug $ "promoting tracked command `" <> identText ident <> "` with " <> show pid
      return $ Tracked pid
    trans a =
      return a

promoteTimedOutExecution ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Elapsed ->
  ExecutionState ->
  m ()
promoteTimedOutExecution ident started =
  checkState
  where
    checkState Pending =
      check
    checkState (Starting _) =
      check
    checkState _ =
      return ()
    check = do
      now <- liftIO timeCurrent
      timeout <- setting Settings.processTimeout
      when (timedOut now timeout) (promoteExecution ident)
    timedOut now timeout =
      now - started > Elapsed (Seconds (fromIntegral timeout))

checkExecuting ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  Execution ->
  m ()
checkExecuting (Execution ident _ _ (ExecutionMonitor state started _ checkPending)) = do
  catchAny (showDebug "checking execution:") $ check state
  promoteTimedOutExecution ident started state
  where
    check Pending =
      update =<< query
    check (Starting _) =
      updateStarting =<< query
    check (Tracked pid) =
      checkTracked ident pid
    check _ =
      return ()
    query =
      liftIO checkPending
    updateStarting s@(Starting _) =
      update s
    updateStarting _ =
      return ()
    update =
      setExecutionState ident

handleEvent ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  MonitorEvent ->
  m ()
handleEvent (CommandOutput ident bytes) =
  logDebug @Text ("command output: " <> decodeUtf8 (sanitizeOutput bytes)) *>
  appendLog ident (sanitizeOutput bytes)
handleEvent Tick =
  traverse_ checkExecuting =<< getL @CommandState CommandState.executing

listenerErrorReport :: Socket -> IOException -> ErrorReport
listenerErrorReport sock ex =
  ErrorReport "command monitor failed" ["exception in output listener:", show ex, "socket: " <> show sock] DEBUG

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
  whenLeft () result (processErrorReport' "monitor" . listenerErrorReport sock)

listen ::
  MonadRibo m =>
  Nvim m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  Ident ->
  Path Abs File ->
  TMChan MonitorEvent ->
  m ()
listen cmdIdent logPath listenChan =
  either failure success =<< tryAny (socketBind logPath)
  where
    success sock = do
      logDebug $ "listening on socket at " <> toFilePath logPath
      storeOutputSocket sock cmdIdent
      void $ forkListener sock
    failure err =
      logDebug $ "could not bind command output socket for `" <> identText cmdIdent <> "`: " <> show err
    forkListener sock =
      fork $ listener cmdIdent sock listenChan

runMonitor ::
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
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
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  m (TMChan MonitorEvent)
startMonitor = do
  chan <- atomically newTMChan
  void $ fork $ runMonitor chan
  void $ fork $ runClock chan
  setL @CommandState CommandState.monitorChan (Just chan)
  return chan

ensureMonitor ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  m (TMChan MonitorEvent)
ensureMonitor = do
  current <- getL @CommandState CommandState.monitorChan
  maybe startMonitor return current

monitorCommand ::
  MonadRibo m =>
  NvimE e m =>
  MonadDeepError e SettingError m =>
  MonadBaseControl IO m =>
  MonadIO m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Path Abs File ->
  m ()
monitorCommand cmdIdent logPath =
  listen cmdIdent logPath =<< ensureMonitor
