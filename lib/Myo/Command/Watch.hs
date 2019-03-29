module Myo.Command.Watch where

import Chiasma.Data.Ident (Ident)
import Conduit (mapC, mapMC, runConduit, sinkNull, (.|))
import Control.Concurrent.Lifted (fork)
import Control.Exception (IOException)
import Control.Exception.Lifted (try)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.DeepState (MonadDeepState, getsL, modify, setL)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import Data.ByteString.Internal (packChars)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString.Search as ByteString (replace)
import Data.Conduit.Network.Unix (sourceSocket)
import Data.Conduit.TMChan (TMChan, newTMChan, sinkTMChan, sourceTMChan)
import Data.Either.Combinators (whenLeft)
import Data.Functor (void)
import Network.Socket (Socket)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report (processErrorReport')
import Ribosome.Error.Report.Class (ReportError)
import System.Log (Priority(NOTICE))
import UnliftIO (atomically)
import UnliftIO.Directory (doesPathExist)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (watcherChan)
import Myo.Command.Log (appendLog)
import Myo.Network.Socket (socketBind)
import Myo.Ui.Data.PaneOutput (PaneOutput(PaneOutput))

strictByteString :: LB.ByteString -> ByteString
strictByteString =
  ByteString.concat . LB.toChunks

replace :: String -> String -> ByteString -> ByteString
replace from to =
  strictByteString . ByteString.replace (packChars from) (packChars to)

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
  whenLeft result (processErrorReport' "watcher" . listenerErrorReport)

listen ::
  (MonadRibo m, Nvim m, MonadIO m, MonadBaseControl IO m) =>
  Ident ->
  FilePath ->
  TMChan PaneOutput ->
  m ()
listen ident logPath listenChan = do
  -- liftBase $ putStrLn $ "listening on socket at " ++ logPath
  sock <- socketBind logPath
  void $ fork $ listener ident sock listenChan

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
watchPane ident logPath = do
  chan <- ensureWatcher
  listen ident logPath chan
