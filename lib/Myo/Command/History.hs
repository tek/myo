{-# LANGUAGE QuasiQuotes #-}

module Myo.Command.History where

import Chiasma.Data.Ident (sameIdent)
import Control.Lens (Lens', element, filtered, firstOf, folded, view, views)
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

import Myo.Command.Command (mayCommandBy)
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.Command as Command (displayName, ident, skipHistory)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (
  CommandError(NoSuchHistoryIndex, NoSuchHistoryIdent, NoSuchCommand),
  )
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry(HistoryEntry))
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (command)
import qualified Myo.Settings as Settings (proteomeMainName, proteomeMainType)

proteomePath ::
  NvimE e m =>
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
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
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  m ()
loadHistory = do
  showDebug "myo-persist-subpath" =<< subPath
  lockOrSkip "load-history" . loadHistoryFrom =<< subPath

duplicateHistoryEntry :: Command -> HistoryEntry -> Bool
duplicateHistoryEntry cmd (HistoryEntry historyCmd) =
  sameIdent cmd historyCmd

pushHistory ::
  NvimE e m =>
  MonadIO m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadThrow m =>
  Command ->
  m ()
pushHistory cmd =
  unless (view Command.skipHistory cmd) $ do
    modifyL @CommandState CommandState.history prep
    storeHistory
  where
    prep es = HistoryEntry cmd : filter (not . duplicateHistoryEntry cmd) es

lookupHistoryIndex ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Int ->
  m Command
lookupHistoryIndex index =
  view HistoryEntry.command <$> (err =<< gets @CommandState (firstOf (CommandState.history . element index)))
  where
    err = hoistMaybe (CommandError.NoSuchHistoryIndex index)

lookupHistoryIdent ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Ident ->
  m Command
lookupHistoryIdent ident =
  view HistoryEntry.command <$> (err =<< gets @CommandState lens)
  where
    lens =
      firstOf (CommandState.history . folded . filtered (sameIdent ident))
    err =
      hoistMaybe (CommandError.NoSuchHistoryIdent (show ident))

lookupHistory ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Either Ident Int ->
  m Command
lookupHistory =
  either lookupHistoryIdent lookupHistoryIndex

mayHistoryBy ::
  Eq a =>
  MonadDeepState s CommandState m =>
  Lens' Command a ->
  a ->
  m (Maybe Command)
mayHistoryBy lens a =
  fmap (view HistoryEntry.command) <$> gets @CommandState entryLens
  where
    entryLens =
      firstOf (CommandState.history . folded . filtered (views (HistoryEntry.command . lens) (a ==)))

historyBy ::
  Eq a =>
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Text ->
  Lens' Command a ->
  a ->
  m Command
historyBy ident lens =
  err <=< mayHistoryBy lens
  where
    err =
      hoistMaybe (CommandError.NoSuchHistoryIdent ident)

mayCommandOrHistoryBy ::
  Eq a =>
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Lens' Command a ->
  a ->
  m (Maybe Command)
mayCommandOrHistoryBy lens a =
  hist =<< mayCommandBy lens a
  where
    hist =
      maybe (mayHistoryBy lens a) (return . Just)

commandOrHistoryBy ::
  Eq a =>
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Text ->
  Lens' Command a ->
  a ->
  m Command
commandOrHistoryBy ident lens a =
  err =<< mayCommandOrHistoryBy lens a
  where
    err =
      hoistMaybe (CommandError.NoSuchCommand "commandOrHistoryBy" ident)

commandOrHistoryByIdent ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Ident ->
  m Command
commandOrHistoryByIdent ident =
  commandOrHistoryBy (show ident) Command.ident ident

displayNameByIdent ::
  MonadDeepState s CommandState m =>
  MonadDeepError e CommandError m =>
  Ident ->
  m Text
displayNameByIdent ident =
  select <$> mayCommandOrHistoryBy Command.ident ident
  where
    select =
      fromMaybe (show ident) . (>>= view Command.displayName)
