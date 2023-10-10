module Myo.Interpreter.Commands where

import Conc (interpretAtomic)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Map.Strict as Map

import Myo.Command.Commands (matcher)
import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError (NoSuchCommand))
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Data.CommandQuery
import Myo.Data.CommandQuery (CommandQuery, CommandQueryDomain (..), CommandQueryField)
import Myo.Effect.Commands (Commands (..))
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)
import Myo.Interpreter.History (interpretHistoryNull, interpretHistoryTransient)

type CommandState = Map CommandId Command

commandList ::
  Member (AtomicState CommandState) r =>
  Sem r [Command]
commandList =
  atomicGets Map.elems

queryCommand ::
  Member (AtomicState CommandState) r =>
  CommandQueryField ->
  CommandQueryDomain ->
  Sem r (Maybe Command)
queryCommand field = \case
  QueryCommands -> run
  QueryHistory -> pure Nothing
  QueryBoth -> run
  where
    run = matcher id field <$> commandList

queryHistory ::
  Member (History !! e) r =>
  CommandQuery ->
  Sem r (Maybe Command)
queryHistory query =
  rightToMaybe <$> resumeEither (History.queryCommand query)

runQuery ::
  Members [History !! e, AtomicState CommandState] r =>
  CommandQuery ->
  Sem r (Maybe Command)
runQuery query =
  runMaybeT do
    MaybeT (queryCommand query.field query.domain) <|> MaybeT (queryHistory query)

interpretCommandsWith ::
  Members [History !! e, Embed IO] r =>
  [Command] ->
  InterpreterFor (Commands !! CommandError) r
interpretCommandsWith initial =
  interpretAtomic (Map.fromList (initial <&> \ c -> (c.ident, c))) .
  interpretResumable \case
    All ->
      commandList
    Latest ->
      resumeHoistAs CommandError.NoHistory (History.latest <&> \ e -> e.command)
    Query query ->
      stopNote (NoSuchCommand query) =<< runQuery query
    Lookup query ->
      runQuery query
    Add cmd ->
      atomicModify' (at cmd.ident ?~ cmd)
  . raiseUnder

interpretCommands ::
  Members [History !! e, Embed IO] r =>
  InterpreterFor (Commands !! CommandError) r
interpretCommands =
  interpretCommandsWith []

interpretCommandsNoHistory ::
  Member (Embed IO) r =>
  InterpretersFor [Commands !! CommandError, History !! RunError] r
interpretCommandsNoHistory =
  interpretHistoryNull .
  interpretCommands

interpretCommandsTransient ::
  Members [Log, Embed IO] r =>
  [HistoryEntry] ->
  InterpretersFor [Commands !! CommandError, History !! RunError] r
interpretCommandsTransient history =
  interpretHistoryTransient history .
  interpretCommands
