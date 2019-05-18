module Myo.Command.History where

import Chiasma.Data.Ident (sameIdent)
import qualified Control.Lens as Lens (element, firstOf)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.DeepError (hoistMaybe)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.Trans.Control (MonadBaseControl)
import Path (Dir, File, Path, Rel, dirname, parent, parseRelDir, relfile, (</>))
import Path.IO (getCurrentDir)
import Ribosome.Config.Setting (settingMaybe)
import Ribosome.Control.Lock (lockOrSkip)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Persist (mayPersistLoad, persistStore)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError(NoSuchHistoryIndex, NoHistory))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (HistoryEntry(command))
import qualified Myo.Settings as Settings (proteomeMainName, proteomeMainType)

proteomePath ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  m (Maybe (Path Rel Dir))
proteomePath =
  runMaybeT ((</>) <$> fetch Settings.proteomeMainType <*> fetch Settings.proteomeMainName)
  where
    fetch s =
      MaybeT $ traverse (parseRelDir . toString) =<< settingMaybe s

fsPath ::
  MonadIO m =>
  m (Path Rel Dir)
fsPath = do
  current <- getCurrentDir
  return $ dirname (parent current) </> dirname current

subPath ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  m (Path Rel File)
subPath =
  (</> [relfile|history|]) <$> (maybe fsPath return =<< proteomePath)

history ::
  MonadDeepState s CommandState m =>
  m [HistoryEntry]
history =
  getL @CommandState CommandState.history

storeHistoryAt ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  Path Rel File ->
  m ()
storeHistoryAt path =
  persistStore path =<< history

storeHistory ::
  NvimE e m =>
  MonadRibo m =>
  MonadThrow m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  m ()
storeHistory =
  lockOrSkip "store-history" . storeHistoryAt =<< subPath

loadHistoryFrom ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  Path Rel File ->
  m ()
loadHistoryFrom path =
  traverse_ (setL @CommandState CommandState.history) =<< mayPersistLoad path

loadHistory ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  m ()
loadHistory =
  lockOrSkip "load-history" . loadHistoryFrom =<< subPath

pushHistory ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  Command ->
  m ()
pushHistory cmd = do
  modifyL @CommandState CommandState.history prep
  storeHistory
  where
    prep es = HistoryEntry cmd : filter (not . sameIdent cmd) es

lookupHistory ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Int ->
  m Command
lookupHistory index =
  HistoryEntry.command <$> (err =<< gets @CommandState (Lens.firstOf (CommandState.history . Lens.element index)))
  where
    err = hoistMaybe (CommandError.NoSuchHistoryIndex index)
