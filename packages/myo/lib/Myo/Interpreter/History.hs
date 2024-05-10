module Myo.Interpreter.History where

import Conc (Lock, interpretAtomic, interpretLockReentrant, lockOrSkip_)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List.Extra (nubOrdOn)
import qualified Data.Set as Set
import Exon (exon)
import qualified Log
import Path (Dir, File, Path, Rel, dirname, parent, relfile, (</>))
import Ribosome (Persist, PersistError, Rpc, RpcError, SettingError, Settings, pathText)
import Ribosome.Api (nvimCwd)
import qualified Ribosome.Persist as Persist
import qualified Ribosome.Settings as Settings

import Myo.Command.Commands (matcher)
import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command (ident))
import Myo.Command.Data.CommandError (CommandError (NoHistory, NoSuchCommand))
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (ExecutionParams (ExecutionParams), HistoryEntry (HistoryEntry))
import qualified Myo.Command.Data.HistoryState
import Myo.Command.Data.HistoryState (HistoryState (HistoryState))
import Myo.Command.Data.LoadHistory (LoadHistory)
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.StoreHistory (StoreHistory)
import Myo.Data.CommandId (CommandId, commandIdText)
import qualified Myo.Data.CommandQuery
import Myo.Data.CommandQuery (CommandQuery, CommandQueryDomain (..), CommandQueryField (..))
import Myo.Effect.History (History (All, Latest, Load, Push, Query, Remove))
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
  (</> [relfile|history.json|]) <$> (maybe fsPath pure =<< proteomePath)

getHistory ::
  Member (AtomicState HistoryState) r =>
  Sem r [HistoryEntry]
getHistory =
  atomicGets (.history)

setHistory ::
  Member (AtomicState HistoryState) r =>
  [HistoryEntry] ->
  Sem r ()
setHistory =
  atomicSet #history . filter (not . null . (.execution.compiled))

removeHistoryEntries ::
  Member (AtomicState HistoryState) r =>
  NonEmpty CommandId ->
  Sem r ()
removeHistoryEntries targets =
  atomicModify' (#history %~ \ old -> foldr check [] old)
  where
    check entry z | elem entry.command.ident targetSet = z
                  | otherwise = entry : z
    targetSet = Set.fromList (toList targets)

storeHistoryAt ::
  Members [Persist [HistoryEntry], AtomicState HistoryState] r =>
  Path Rel File ->
  Sem r ()
storeHistoryAt path =
  Persist.store (Just path) =<< getHistory

storeHistory ::
  Members [Rpc, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry], AtomicState HistoryState, Lock @@ StoreHistory, Resource] r =>
  Sem r ()
storeHistory =
  tag $ lockOrSkip_ do
    storeHistoryAt =<< subPath

loadHistoryFrom ::
  Members [Persist [HistoryEntry], AtomicState HistoryState, Log] r =>
  Path Rel File ->
  Sem r ()
loadHistoryFrom path = do
  Log.debug [exon|Loading history from '#{pathText path}'|]
  traverse_ setHistory =<< Persist.load (Just path)

loadHistory ::
  Members [Rpc, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry], AtomicState HistoryState, Lock @@ LoadHistory, Resource, Log] r =>
  Sem r ()
loadHistory =
  tag $ lockOrSkip_ do
    loadHistoryFrom =<< subPath

pushHistory ::
  Members [AtomicState HistoryState, Log] r =>
  Command ->
  CommandId ->
  ParamValues ->
  [Text] ->
  Sem r ()
pushHistory cmd exeId params compiled =
  unless cmd.skipHistory do
    Log.debug [exon|Pushing command '#{commandIdText cmd.ident}' to history, params: #{show params}|]
    atomicModify' (#history %~ prep)
  where
    prep es = nubOrdOn entryKey (HistoryEntry cmd (ExecutionParams exeId compiled params) : es)

    entryKey (HistoryEntry _ execution) = execution.compiled

byExecutionId :: CommandQueryField -> [HistoryEntry] -> Maybe HistoryEntry
byExecutionId = \case
  CommandById target -> find \case
    HistoryEntry {execution} -> execution.id == target
  _ -> const Nothing

queryCommand ::
  Member (AtomicState HistoryState) r =>
  CommandQueryField ->
  Sem r (Maybe HistoryEntry)
queryCommand field =
  atomicGets \ HistoryState {history} -> matcher (.command) field history <|> byExecutionId field history

runQuery ::
  Member (AtomicState HistoryState) r =>
  CommandQueryField ->
  CommandQueryDomain ->
  Sem r (Maybe HistoryEntry)
runQuery field = \case
  QueryCommands -> pure Nothing
  QueryHistory -> queryCommand field
  QueryBoth -> queryCommand field

latest ::
  Members [AtomicState HistoryState, Stop RunError] r =>
  Sem r HistoryEntry
latest =
  stopNote (RunError.Command NoHistory) . head =<< atomicGets (.history)

queryOrStop ::
  Members [AtomicState HistoryState, Stop RunError] r =>
  CommandQuery ->
  Sem r HistoryEntry
queryOrStop query =
  stopNote (RunError.Command (NoSuchCommand query)) =<< runQuery query.field query.domain

type HistoryEffects =
  [
    AtomicState HistoryState,
    Lock @@ StoreHistory,
    Lock @@ LoadHistory
  ]

withLocalEffects ::
  Members [Mask, Resource, Race, Embed IO] r =>
  [HistoryEntry] ->
  InterpreterFor (History !! RunError) (HistoryEffects ++ r) ->
  InterpreterFor (History !! RunError) r
withLocalEffects initial int ma =
  interpretLockReentrant $ untag $
  interpretLockReentrant $ untag $
  interpretAtomic (HistoryState initial) $
  int (raiseUnder3 ma)

-- TODO add HistoryError
interpretHistoryWith ::
  Members [Mask, Resource, Race, Embed IO] r =>
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry] !! PersistError, Log] r =>
  [HistoryEntry] ->
  InterpreterFor (History !! RunError) r
interpretHistoryWith initial =
  withLocalEffects initial $
  interpretResumable \case
    All ->
      getHistory
    Latest ->
      latest
    Push cmd exeId params compiled ->
      resumeHoist @_ @Rpc RunError.Rpc $
      resumeHoist RunError.Persist do
        pushHistory cmd exeId params compiled
        unless cmd.skipHistory do
          storeHistory
    Remove idents ->
      removeHistoryEntries idents
    Load ->
      resumeHoist @_ @Rpc RunError.Rpc $
      resumeHoist RunError.Persist $
      loadHistory
    Query query ->
      queryOrStop query

interpretHistory ::
  Members [Mask, Resource, Race, Embed IO] r =>
  Members [Rpc !! RpcError, Settings !! SettingError] r =>
  Members [Persist [HistoryEntry] !! PersistError, Log] r =>
  InterpreterFor (History !! RunError) r
interpretHistory =
  interpretHistoryWith def

interpretHistoryTransient ::
  Members [Log, Embed IO] r =>
  [HistoryEntry] ->
  InterpreterFor (History !! RunError) r
interpretHistoryTransient initial =
  interpretAtomic (HistoryState initial) .
  interpretResumable \case
    All ->
      getHistory
    Latest ->
      latest
    Push cmd exeId params compiled ->
      pushHistory cmd exeId params compiled
    Remove idents ->
      removeHistoryEntries idents
    Load ->
      unit
    Query query ->
      queryOrStop query
  . raiseUnder

interpretHistoryNull :: InterpreterFor (History !! RunError) r
interpretHistoryNull =
  interpretResumable \case
    All -> pure []
    Latest -> stop (RunError.Command NoHistory)
    Push _ _ _ _ -> unit
    Remove _ -> unit
    Load -> unit
    Query query -> stop (RunError.Command (NoSuchCommand query))
