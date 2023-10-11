module Myo.Command.Interpreter.Backend.Process where

import qualified Data.List as List
import Exon (exon)
import qualified Log
import Path (toFilePath)
import qualified Polysemy.Process as Process
import Polysemy.Process (
  OutputPipe (Stderr, Stdout),
  Process,
  ProcessError (Exit, StartFailed, Unknown),
  SysProcConf,
  interpretProcessInputText,
  interpretProcessNative,
  interpretProcessOutputLeft,
  interpretProcessOutputLines,
  interpretProcessOutputRight,
  interpretProcessOutputTextLines,
  withProcess,
  )
import Ribosome (Rpc, RpcError)
import Ribosome.Api (nvimCwd)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, setWorkingDir)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (System, UiSystem))
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.ProcessTask (ProcessTask (ProcessTask))

outputEvent ::
  Members [CommandLog, State [Text]] r =>
  CommandId ->
  Maybe Int ->
  Either Text ByteString ->
  Sem r ()
outputEvent ident maxLogBytes = \case
  Right out -> CommandLog.append ident maxLogBytes out
  Left out -> modify' (out :)

runProcess ::
  Members [Stop RunError, Log] r =>
  Members [CommandLog, Scoped (String, [String]) (Process Text (Either Text ByteString)) !! ProcessError] r =>
  ProcessTask ->
  Sem r ()
runProcess (ProcessTask ident cmd maxLogBytes) = do
  Log.debug [exon|Starting subprocess for '#{commandIdText ident}': #{cmdline}|]
  evalState mempty $ resuming @_ @(Scoped _ _) checkError $ withProcess cmd do
    forever do
      outputEvent ident maxLogBytes =<< Process.recv
  where
    cmdline =
      toText [exon|#{fst cmd} #{List.unwords (snd cmd)}|]
    checkError = \case
      Unknown msg -> failure (show msg)
      StartFailed msg -> failure (show msg)
      Exit ExitSuccess -> unit
      Exit (ExitFailure code) -> failure [exon|exit code #{show code}|]
    failure msg = do
      Log.debug [exon|Subprocess for '#{commandIdText ident}' failed: #{msg}|]
      stop . RunError.SubprocFailed msg =<< get

conf ::
  Member (Rpc !! RpcError) r =>
  (String, [String]) ->
  Sem r SysProcConf
conf (exe, args) = do
  cwd <- Nothing <! (Just <$> nvimCwd)
  pure (maybe id (setWorkingDir . toFilePath) cwd (proc exe args))

processTask :: Command -> [Text] -> Maybe ProcessTask
processTask Command {ident, maxLogBytes} = \case
  [l] ->
    case List.words (toString l) of
      (h : t) -> Just (ProcessTask ident (h, t) maxLogBytes)
      [] -> Nothing
  _ ->
    Nothing

acceptCommand ::
  RunTask ->
  Sem r (Maybe ProcessTask)
acceptCommand = \case
  RunTask _ cmd System compiled _ ->
    pure (processTask cmd compiled)
  RunTask _ cmd (UiSystem _) compiled _ ->
    pure (processTask cmd compiled)
  _ ->
    pure Nothing

interpretBackendProcess ::
  Members [Backend !! RunError, CommandLog, Log] r =>
  Member (Scoped (String, [String]) (Process Text (Either Text ByteString)) !! ProcessError) r =>
  Sem r a ->
  Sem r a
interpretBackendProcess =
  interceptBackend acceptCommand runProcess (captureUnsupported "process") unit

interpretBackendProcessNative ::
  Members [Backend !! RunError, Rpc !! RpcError, CommandLog, Log, Resource, Race, Async, Embed IO] r =>
  Sem r a ->
  Sem r a
interpretBackendProcessNative =
  interpretProcessOutputTextLines @'Stderr .
  interpretProcessOutputLeft @'Stderr .
  interpretProcessOutputLines @'Stdout .
  interpretProcessOutputRight @'Stdout .
  interpretProcessInputText .
  interpretProcessNative def (fmap Right . conf) .
  interpretBackendProcess .
  insertAt @0
