module Myo.Command.Log where

import Chiasma.Command.Pane (pipePane)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.TmuxApi (Tmux)
import qualified Data.ByteString as ByteString (null)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text (concatMap, lines, singleton)
import Exon (exon)
import qualified Log
import Network.Socket (Socket)
import Path (Abs, File, Path, parseRelFile, toFilePath, (</>))
import Ribosome (Handler, RpcError, Scratch, resumeHandlerError)
import qualified Ribosome.Scratch as Scratch
import Ribosome.Scratch (ScratchOptions (focus, name))

import Myo.Command.Command (mainCommand, mainCommandIdent)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell))
import Myo.Command.Data.CommandLog (CommandLog (CommandLog))
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState, Logs)
import Myo.Command.Data.LogDir (LogDir (LogDir))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.History (commandOrHistoryBy, commandOrHistoryByIdent, mayCommandOrHistoryByIdent)
import Myo.Network.Socket (socketBind)

logPathLens :: Ident -> Lens' CommandState (Maybe (Path Abs File))
logPathLens ident =
  #logPaths . at ident

logPathByIdent ::
  Member (AtomicState CommandState) r =>
  Ident ->
  Sem r (Maybe (Path Abs File))
logPathByIdent ident =
  atomicGets (view (logPathLens ident))

logPath ::
  Member (Reader LogDir) r =>
  Ident ->
  Sem r (Maybe (Path Abs File))
logPath ident =
  ask <&> \ (LogDir base) ->
    (base </>) <$> parseRelFile [exon|pane-#{toString (identText ident)}|]

insertLogPath ::
  Members [Reader LogDir, AtomicState CommandState, Stop RunError] r =>
  Ident ->
  Sem r (Path Abs File)
insertLogPath ident = do
  p <- stopNote invalidIdent =<< logPath ident
  atomicModify' (logPathLens ident ?~ p)
  pure p
  where
    invalidIdent =
      RunError.Command (CommandError.Misc ("invalid command ident: " <> identT))
    identT =
      identText ident

commandLogPath ::
  Members [Reader LogDir, AtomicState CommandState, Stop RunError] r =>
  Ident ->
  Sem r (Path Abs File)
commandLogPath ident = do
  existing <- logPathByIdent ident
  maybe (insertLogPath ident) pure existing

commandLogSocketBind ::
  Members [Reader LogDir, AtomicState CommandState, Stop RunError, Embed IO] r =>
  Ident ->
  Sem r Socket
commandLogSocketBind =
  socketBind <=< commandLogPath

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

logLens :: Ident -> Lens' CommandState (Maybe CommandLog)
logLens ident =
  #logs . at ident

commandLogs :: Member (AtomicState CommandState) r => Sem r Logs
commandLogs =
  atomicGets CommandState.logs

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
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Lens' Command a ->
  a ->
  Sem r (Maybe CommandLog)
commandLogBy ident lens a = do
  cmd <- commandOrHistoryBy ident lens a
  logIdent <- mainCommandOrHistoryIdent (Command.ident cmd)
  atomicView (logLens logIdent)

commandLog ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Ident ->
  Sem r (Maybe CommandLog)
commandLog ident =
  commandLogBy (show ident) #ident ident

commandLogByName ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Text ->
  Sem r (Maybe CommandLog)
commandLogByName name =
  commandLogBy name #displayName (Just name)

appendLog ::
  Member (AtomicState CommandState) r =>
  Ident ->
  ByteString ->
  Sem r ()
appendLog ident bytes =
  atomicModify' (logLens ident %~ append)
  where
    append (Just (CommandLog prev cur)) =
      Just (CommandLog prev (cur <> bytes))
    append Nothing =
      Just (CommandLog [] bytes)

pushCommandLog ::
  Members [AtomicState CommandState, Log] r =>
  Ident ->
  Sem r ()
pushCommandLog ident = do
  logIdent <- mayMainCommandOrHistory ident
  Log.debug [exon|pushing command log for `#{identText logIdent}`|]
  atomicModify' (logLens logIdent %~ fmap push)
  where
    push (CommandLog prev cur) | ByteString.null cur =
      CommandLog prev cur
    push (CommandLog prev cur) =
      CommandLog (cur : prev) ""

pushCommandLogs ::
  Members [AtomicState CommandState, Log] r =>
  Sem r ()
pushCommandLogs = do
  idents <- atomicGets (Map.keys . CommandState.logs)
  traverse_ pushCommandLog idents

myoLogs ::
  Members [AtomicState CommandState, Scratch !! RpcError, Log] r =>
  Handler r ()
myoLogs =
  resumeHandlerError @Scratch do
    logs <- commandLogs
    void $ Scratch.show ("# Command Logs" : "" : intercalate [""] (logLines logs)) options
  where
    options =
      def {
        name = "myo-command-logs",
        focus = True
      }
    logLines logs =
      uncurry formatLog <$> Map.toList logs
    formatLog ident (CommandLog previous current) =
      ("## " <> identText ident) : "" : split current ++ intercalate [""] (split <$> previous)
    split =
      Text.lines . decodeUtf8
