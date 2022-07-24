module Myo.Command.Log where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.TmuxApi (Tmux)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text (concatMap, lines, singleton)
import Path (Abs, File, Path, toFilePath)
import Ribosome (Handler, RpcError, Scratch, resumeHandlerError, scratch)
import qualified Ribosome.Scratch as Scratch

import Myo.Command.Command (mainCommand, mainCommandIdent)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.History (commandOrHistoryBy, commandOrHistoryByIdent, mayCommandOrHistoryByIdent)

pipePaneToSocket ::
  Member Tmux r =>
  PaneId ->
  Path Abs File ->
  Sem r ()
pipePaneToSocket paneId path =
  pipePane paneId cmd
  where
    cmd = "'socat STDIN UNIX-SENDTO:" <> (Text.concatMap escape . toText . toFilePath) path <> "'"
    escape c = prefix c <> Text.singleton c
    prefix c | isAlphaNum c = ""
    prefix _  = "\\"

mainCommandOrHistory ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Ident ->
  Sem r Command
mainCommandOrHistory context ident = do
  cmd <- commandOrHistoryByIdent context ident
  recurse cmd (cmd ^. #interpreter)
  where
    recurse cmd = \case
      Shell target ->
        mainCommand target
      _ ->
        pure cmd

mainCommandOrHistoryIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Ident ->
  Sem r Ident
mainCommandOrHistoryIdent context ident =
  recurse . Command.interpreter =<< commandOrHistoryByIdent context ident
  where
    recurse (Shell target) =
      mainCommandIdent target
    recurse _ =
      pure ident

mayMainCommandOrHistory ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r Ident
mayMainCommandOrHistory ident =
  recurse . fmap Command.interpreter =<< mayCommandOrHistoryByIdent ident
  where
    recurse (Just (Shell target)) =
      mayMainCommandOrHistory target
    recurse _ =
      pure ident

commandLogBy ::
  Eq a =>
  Members [CommandLog, AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Text ->
  Lens' Command a ->
  a ->
  Sem r (Maybe Text)
commandLogBy context ident lens a = do
  cmd <- commandOrHistoryBy context ident lens a
  logIdent <- mainCommandOrHistoryIdent context (Command.ident cmd)
  CommandLog.get logIdent

-- commandLog ::
--   Members [AtomicState CommandState, Stop CommandError] r =>
--   Ident ->
--   Sem r (Maybe CommandLog)
-- commandLog ident =
--   commandLogBy (show ident) #ident ident

commandLogByName ::
  Members [CommandLog, AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Text ->
  Sem r (Maybe Text)
commandLogByName context name =
  commandLogBy context name #displayName (Just name)

myoLogs ::
  Members [CommandLog, AtomicState CommandState, Scratch !! RpcError, Log] r =>
  Handler r ()
myoLogs =
  void $ resumeHandlerError @Scratch do
    logs <- CommandLog.all
    Scratch.show ("# Command Logs" : "" : intercalate [""] (logLines logs)) options
  where
    options =
      (scratch "myo-command-logs") { Scratch.focus = True, Scratch.filetype = Just "markdown" }
    logLines logs =
      uncurry formatLog <$> Map.toList logs
    formatLog ident log =
      ("## " <> identText ident) : "" : "```" : Text.lines log ++ ["```"]
