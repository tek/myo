module Myo.Command.Watch where

import Chiasma.Data.Ident (Ident)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Monad.DeepState (MonadDeepState, getsL, setL)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString.Search as ByteString (replace)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan)
import Data.Functor (void)
import Network.Socket (Socket)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report (processErrorReport')
import System.Log (Priority(NOTICE))

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (watcherChan)
import Myo.Command.Log (appendLog)
import qualified Myo.Log as Log (debug)
import Myo.Network.Socket (socketBind)
import Myo.Ui.Data.PaneOutput (PaneOutput(PaneOutput))

strictByteString :: LB.ByteString -> ByteString
strictByteString =
  ByteString.concat . LB.toChunks

replace :: Text -> Text -> ByteString -> ByteString
replace from to =
  strictByteString . ByteString.replace (encodeUtf8 from :: ByteString) (encodeUtf8 to :: ByteString)

sanitizeOutput :: ByteString -> ByteString
sanitizeOutput =
  replace "\r\n" "\n"

handleOutput :: (MonadDeepState s CommandState m, MonadIO m) => PaneOutput -> m ()
handleOutput (PaneOutput ident bytes) =
  appendLog ident (sanitizeOutput bytes)

listenerErrorReport :: IOException -> ErrorReport
listenerErrorReport ex = ErrorReport "command output watcher failed" ["exception in pane listener:", show ex] NOTICE

listener ::
  (MonadRibo m, Nvim m, MonadIO m, MonadBaseControl IO m) =>
  Ident ->
  Socket ->
  TMChan PaneOutput ->
  m ()
listener ident sock listenChan = do
  result <- try $ runConduit $ sourceSocket sock .| mapC (PaneOutput ident) .| sinkTMChan listenChan
  whenLeft () result (processErrorReport' "watcher" . listenerErrorReport)

listen ::
  (MonadRibo m, Nvim m, MonadIO m, MonadBaseControl IO m) =>
  Ident ->
  FilePath ->
  TMChan PaneOutput ->
  m ()
listen ident logPath listenChan = do
  Log.debug $ "listening on socket at " <> logPath
  try (socketBind logPath) >>= \case
    Right sock -> void $ fork $ listener ident sock listenChan
    Left (_ :: IOException) -> return ()

runWatcher ::
  (MonadRibo m, Nvim m, MonadIO m, MonadDeepState s CommandState m, MonadBaseControl IO m) =>
  TMChan PaneOutput ->
  m ()
runWatcher listenChan =
  runConduit $ sourceTMChan listenChan .| mapMC handleOutput .| sinkNull

startWatcher ::
  ∀ s m.
  (MonadRibo m, Nvim m, MonadBaseControl IO m, MonadIO m, MonadDeepState s CommandState m) =>
  m (TMChan PaneOutput)
startWatcher = do
  chan <- atomically newTMChan
  void $ fork $ runWatcher chan
  setL @CommandState CommandState.watcherChan (Just chan)
  return chan

ensureWatcher ::
  ∀ s m.
  (MonadRibo m, Nvim m, MonadBaseControl IO m, MonadIO m, MonadDeepState s CommandState m) =>
  m (TMChan PaneOutput)
ensureWatcher = do
  current <- getsL @CommandState CommandState.watcherChan
  maybe startWatcher return current

watchPane ::
  (MonadRibo m, Nvim m, MonadBaseControl IO m, MonadIO m, MonadDeepState s CommandState m) =>
  Ident ->
  FilePath ->
  m ()
watchPane ident logPath =
  listen ident logPath =<< ensureWatcher
