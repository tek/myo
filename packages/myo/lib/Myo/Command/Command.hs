module Myo.Command.Command where


import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell, System))
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState (commands))
import qualified Myo.Command.Data.HistoryEntry as HistoryEntry
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId, commandIdText)

mayCommandBy ::
  Eq a =>
  Member (AtomicState CommandState) r =>
  Lens' Command a ->
  a ->
  Sem r (Maybe Command)
mayCommandBy attr a =
  atomicGets commands <&> find \ c -> c ^. attr == a

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
  CommandId ->
  Sem r (Maybe Command)
mayCommandByIdent =
  mayCommandBy #ident

commandByIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  CommandId ->
  Sem r Command
commandByIdent context ident =
  commandBy context (commandIdText ident) #ident ident

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
  byIdent <- mayCommandByIdent (fromText query)
  byName <- mayCommandBy #displayName (Just query)
  noSuchCommand context query (byIdent <|> byName)

systemCommand ::
  Maybe UiTarget ->
  CommandId ->
  [Text] ->
  Command
systemCommand target =
  Command.cons (System target)

shellCommand ::
  CommandId ->
  CommandId ->
  [Text] ->
  Command
shellCommand target =
  Command.cons (Shell target)

latestCommand ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Sem r Command
latestCommand =
  fmap HistoryEntry.command . stopNote CommandError.NoCommands . head =<< atomicGets CommandState.history

mainCommand ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  CommandId ->
  Sem r Command
mainCommand ident =
  check =<< commandByIdent "mainCommand" ident
  where
    check = \case
      Command { interpreter = Shell target } ->
        mainCommand target
      cmd ->
        pure cmd

mainCommandIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  CommandId ->
  Sem r CommandId
mainCommandIdent =
  fmap Command.ident . mainCommand
