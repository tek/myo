module Myo.Command.Log where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.TmuxApi (Tmux)
import Data.Char (isAlphaNum)
import qualified Data.Text as Text (concatMap, singleton)
import Path (Abs, File, Path, toFilePath)

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
  Ident ->
  Sem r Command
mainCommandOrHistory ident = do
  cmd <- commandOrHistoryByIdent ident
  recurse cmd (cmd ^. #interpreter)
  where
    recurse cmd = \case
      Shell target ->
        mainCommand target
      _ ->
        pure cmd

mainCommandOrHistoryIdent ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r Ident
mainCommandOrHistoryIdent ident =
  recurse . Command.interpreter =<< commandOrHistoryByIdent ident
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
  Lens' Command a ->
  a ->
  Sem r (Maybe Text)
commandLogBy ident lens a = do
  cmd <- commandOrHistoryBy ident lens a
  logIdent <- mainCommandOrHistoryIdent (Command.ident cmd)
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
  Sem r (Maybe Text)
commandLogByName name =
  commandLogBy name #displayName (Just name)

-- myoLogs ::
--   Members [AtomicState CommandState, Scratch !! RpcError, Log] r =>
--   Handler r ()
-- myoLogs =
--   resumeHandlerError @Scratch do
--     logs <- commandLogs
--     void $ Scratch.show ("# Command Logs" : "" : intercalate [""] (logLines logs)) options
--   where
--     options =
--       def {
--         name = "myo-command-logs",
--         focus = True
--       }
--     logLines logs =
--       uncurry formatLog <$> Map.toList logs
--     formatLog ident (CommandLog previous current) =
--       ("## " <> identText ident) : "" : split current ++ intercalate [""] (split <$> previous)
--     split =
--       Text.lines . decodeUtf8
