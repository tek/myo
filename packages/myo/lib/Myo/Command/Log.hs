module Myo.Command.Log where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text (lines)
import Ribosome (Handler, RpcError, Scratch, resumeReport, scratch)
import qualified Ribosome.Scratch as Scratch

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell))
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.CommandName (CommandName)
import Myo.Data.CommandQuery (CommandQuery, queryId, queryNameBoth)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

mainCommand ::
  Member Commands r =>
  CommandQuery ->
  Sem r Command
mainCommand query =
  check =<< Commands.query query
  where
    check = \case
      Command {interpreter = Shell target} -> mainCommand (queryId target)
      cmd -> pure cmd

mainCommandId ::
  Member Commands r =>
  CommandQuery ->
  Sem r CommandId
mainCommandId =
  fmap (.ident) . mainCommand

commandLogByName ::
  Members [Commands !! e, CommandLog] r =>
  CommandName ->
  Sem r (Maybe Text)
commandLogByName name =
  resumeOr (mainCommand (queryNameBoth name)) (\ c -> CommandLog.get c.ident) (const (pure Nothing))

myoLogs ::
  Members [CommandLog, Scratch !! RpcError] r =>
  Handler r ()
myoLogs =
  void $ resumeReport @Scratch do
    logs <- CommandLog.all
    Scratch.show ("# Command Logs" : "" : intercalate [""] (logLines logs)) options
  where
    options =
      (scratch "myo-command-logs") { Scratch.focus = True, Scratch.filetype = Just "markdown" }
    logLines logs =
      uncurry formatLog <$> Map.toList logs
    formatLog ident log =
      ("## " <> commandIdText ident) : "" : "```" : Text.lines log ++ ["```"]
