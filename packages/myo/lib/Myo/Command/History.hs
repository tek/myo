module Myo.Command.History where

import Chiasma.Data.Ident (Ident, identText, sameIdent)
import Control.Lens (element, firstOf, views)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.Extra (nubOrdOn)
import Path (Dir, File, Path, Rel, dirname, parent, relfile, (</>))
import Ribosome (Persist, Rpc, SettingError, Settings, lockOrSkip_)
import Ribosome.Api (nvimCwd)
import qualified Ribosome.Persist as Persist
import qualified Ribosome.Settings as Settings

import Myo.Command.Command (mayCommandBy)
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.Command as Command (displayName, skipHistory)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (history)
import Myo.Command.Data.HistoryEntry (HistoryEntry (HistoryEntry))
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry (command)
import Myo.Command.Data.LoadHistoryLock (LoadHistoryLock)
import Myo.Command.Data.StoreHistoryLock (StoreHistoryLock)
import qualified Myo.Settings as Settings

-- TODO use variable watcher for this or simple index by the last two directory segments
proteomePath ::
  Member (Settings !! SettingError) r =>
  Sem r (Maybe (Path Rel Dir))
proteomePath =
  runMaybeT ((</>) <$> fetch Settings.proteomeMainTypeDir <*> fetch Settings.proteomeMainNameDir)
  where
    fetch s =
      MaybeT (Settings.maybe s)

fsPath ::
  Member Rpc r =>
  Sem r (Path Rel Dir)
fsPath = do
  current <- nvimCwd
  pure $ dirname (parent current) </> dirname current

subPath ::
  Members [Rpc, Settings !! SettingError] r =>
  Sem r (Path Rel File)
subPath =
  (</> [relfile|history|]) <$> (maybe fsPath pure =<< proteomePath)

history ::
  Member (AtomicState CommandState) r =>
  Sem r [HistoryEntry]
history =
  atomicGets CommandState.history

storeHistoryAt ::
  Members [Persist [HistoryEntry], AtomicState CommandState] r =>
  Path Rel File ->
  Sem r ()
storeHistoryAt path =
  Persist.store (Just path) =<< history

storeHistory ::
  Members [Rpc, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry], AtomicState CommandState, Sync StoreHistoryLock, Resource] r =>
  Sem r ()
storeHistory =
  lockOrSkip_ @StoreHistoryLock do
    storeHistoryAt =<< subPath

loadHistoryFrom ::
  Members [Persist [HistoryEntry], AtomicState CommandState] r =>
  Path Rel File ->
  Sem r ()
loadHistoryFrom path =
  traverse_ (atomicSet #history) =<< Persist.load (Just path)

loadHistory ::
  Members [Rpc, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry], AtomicState CommandState, Sync LoadHistoryLock, Resource] r =>
  Sem r ()
loadHistory =
  lockOrSkip_ @LoadHistoryLock do
    loadHistoryFrom =<< subPath

duplicateHistoryEntry :: Command -> HistoryEntry -> Bool
duplicateHistoryEntry cmd (HistoryEntry historyCmd) =
  sameIdent cmd historyCmd

pushHistory ::
  Members [Rpc, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry], AtomicState CommandState, Sync StoreHistoryLock, Resource] r =>
  Command ->
  Sem r ()
pushHistory cmd =
  unless (Command.skipHistory cmd) do
    atomicModify' (#history %~ prep)
    storeHistory
  where
    prep es =
      nubOrdOn (duplicateHistoryEntry cmd) (HistoryEntry cmd : es)

lookupHistoryIndex ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Int ->
  Sem r Command
lookupHistoryIndex index =
  HistoryEntry.command <$> (err =<< atomicGets (firstOf (#history . element index)))
  where
    err =
      stopNote (CommandError.NoSuchHistoryIndex index)

lookupHistoryIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r Command
lookupHistoryIdent ident =
  HistoryEntry.command <$> (err =<< atomicGets lens)
  where
    lens =
      firstOf (#history . folded . filtered (sameIdent ident))
    err =
      stopNote (CommandError.NoSuchHistoryIdent (show ident))

lookupHistory ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Either Ident Int ->
  Sem r Command
lookupHistory =
  either lookupHistoryIdent lookupHistoryIndex

mayHistoryBy ::
  Eq a =>
  Member (AtomicState CommandState) r =>
  Lens' Command a ->
  a ->
  Sem r (Maybe Command)
mayHistoryBy lens a =
  fmap HistoryEntry.command <$> atomicGets entryLens
  where
    entryLens =
      firstOf (#history . folded . filtered (views (#command . lens) (a ==)))

historyBy ::
  Eq a =>
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Lens' Command a ->
  a ->
  Sem r Command
historyBy ident lens =
  err <=< mayHistoryBy lens
  where
    err =
      stopNote (CommandError.NoSuchHistoryIdent ident)

mayCommandOrHistoryBy ::
  Eq a =>
  Member (AtomicState CommandState) r =>
  Lens' Command a ->
  a ->
  Sem r (Maybe Command)
mayCommandOrHistoryBy lens a =
  hist =<< mayCommandBy lens a
  where
    hist =
      maybe (mayHistoryBy lens a) (pure . Just)

commandOrHistoryBy ::
  Eq a =>
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Lens' Command a ->
  a ->
  Sem r Command
commandOrHistoryBy ident lens a =
  err =<< mayCommandOrHistoryBy lens a
  where
    err =
      stopNote (CommandError.NoSuchCommand "commandOrHistoryBy" ident)

mayCommandOrHistoryByIdent ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r (Maybe Command)
mayCommandOrHistoryByIdent =
  mayCommandOrHistoryBy #ident

commandOrHistoryByIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r Command
commandOrHistoryByIdent ident =
  commandOrHistoryBy (show ident) #ident ident

displayNameByIdent ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r Text
displayNameByIdent ident =
  select <$> mayCommandOrHistoryBy #ident ident
  where
    select =
      fromMaybe (identText ident) . (>>= Command.displayName)
