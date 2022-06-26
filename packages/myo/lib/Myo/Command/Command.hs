module Myo.Command.Command where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident, identText)
import Control.Lens (firstOf, views)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandError as CommandError (CommandError (NoCommands, NoSuchCommand))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell, System))
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry

mayCommandBy ::
  Eq a =>
  Member (AtomicState CommandState) r =>
  Lens' Command a ->
  a ->
  Sem r (Maybe Command)
mayCommandBy lens a =
  atomicGets f
  where
    f :: CommandState -> Maybe Command
    f = firstOf (#commands . folded . filtered (views lens (a ==)))

noSuchCommand ::
  Member (Stop CommandError) r =>
  Text ->
  Text ->
  Maybe a ->
  Sem r a
noSuchCommand context query =
  stopNote (CommandError.NoSuchCommand context query)

commandBy ::
  Eq a =>
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Text ->
  Lens' Command a ->
  a ->
  Sem r Command
commandBy context ident lens a =
  noSuchCommand context ident =<< mayCommandBy lens a

mayCommandByIdent ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r (Maybe Command)
mayCommandByIdent =
  mayCommandBy #ident

commandByIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Ident ->
  Sem r Command
commandByIdent context ident =
  commandBy context (show ident) #ident ident

commandByName ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Text ->
  Sem r Command
commandByName context name =
  commandBy context name #displayName (Just name)

commandByIdentOrName ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Text ->
  Sem r Command
commandByIdentOrName context query = do
  byIdent <- mayCommandByIdent (Ident.Str query)
  byName <- mayCommandBy #displayName (Just query)
  noSuchCommand context query (byIdent <|> byName)

systemCommand ::
  Maybe Ident ->
  Ident ->
  [Text] ->
  Command
systemCommand target =
  Command.cons (System target)

shellCommand ::
  Ident ->
  Ident ->
  [Text] ->
  Command
shellCommand target =
  Command.cons (Shell target)

latestCommand ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r Command
latestCommand =
  fmap HistoryEntry.command . stopNote CommandError.NoCommands . head =<< atomicGets CommandState.history

mayMainCommand ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r (Maybe Command)
mayMainCommand ident =
  check =<< mayCommandByIdent ident
  where
    check = \case
      Just (Command { interpreter = Shell target }) ->
        mayMainCommand target
      cmd ->
        pure cmd

mainCommand ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r Command
mainCommand ident =
  err =<< mayMainCommand ident
  where
    err =
      stopNote (CommandError.NoSuchCommand "mainCommand" (identText ident))

mayMainCommandIdent ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r (Maybe Ident)
mayMainCommandIdent =
  fmap (fmap Command.ident) . mayMainCommand

mainCommandIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r Ident
mainCommandIdent =
  fmap Command.ident . mainCommand
