module Myo.Command.Interpreter.Backend.Process where

import Conc (PScoped)
import qualified Data.List as List
import Exon (exon)
import qualified Log
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
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc, setWorkingDir)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command, cmdLines))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (System))
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.ProcessTask (ProcessTask (ProcessTask))
import Ribosome.Api (nvimCwd)
import Ribosome (Rpc, RpcError)
import Path (toFilePath)

outputEvent ::
  Members [CommandLog, State [Text]] r =>
  CommandId ->
  Either Text ByteString ->
  Sem r ()
outputEvent ident = \case
  Right out -> CommandLog.append ident out
  Left out -> modify' (out :)

runProcess ::
  ∀ pres r .
  Members [Stop RunError, Log] r =>
  Members [CommandLog, PScoped (String, [String]) pres (Process Text (Either Text ByteString)) !! ProcessError] r =>
  ProcessTask ->
  Sem r ()
runProcess (ProcessTask ident cmd) = do
  Log.debug [exon|Starting subprocess for `#{commandIdText ident}`: #{cmdline}|]
  evalState mempty $ resuming @_ @(PScoped _ _ _) checkError $ withProcess cmd do
    forever do
      outputEvent ident =<< Process.recv
  where
    cmdline =
      toText [exon|#{fst cmd} #{List.unwords (snd cmd)}|]
    checkError = \case
      Unknown msg -> failure (show msg)
      StartFailed msg -> failure (show msg)
      Exit ExitSuccess -> unit
      Exit (ExitFailure code) -> failure [exon|exit code #{show code}|]
    failure msg = do
      Log.debug [exon|Subprocess for `#{commandIdText ident}` failed: #{msg}|]
      stop . RunError.SubprocFailed msg =<< get

conf ::
  Member (Rpc !! RpcError) r =>
  (String, [String]) ->
  Sem r SysProcConf
conf (exe, args) = do
  cwd <- Nothing <! (Just <$> nvimCwd)
  pure (maybe id (setWorkingDir . toFilePath) cwd (proc exe args))

processTask :: Command -> Maybe ProcessTask
processTask = \case
  Command {ident, cmdLines = [l]} ->
    case List.words (toString l) of
      (h : t) -> Just (ProcessTask ident (h, t))
      [] -> Nothing
  _ ->
    Nothing

acceptCommand ::
  RunTask ->
  Sem r (Maybe ProcessTask)
acceptCommand = \case
  RunTask cmd System ->
    pure (processTask cmd)
  _ ->
    pure Nothing

interpretBackendProcess ::
  ∀ pres r a .
  Members [Backend !! RunError, CommandLog, Log] r =>
  Member (PScoped (String, [String]) pres (Process Text (Either Text ByteString)) !! ProcessError) r =>
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
  interpretProcessNative def conf .
  interpretBackendProcess .
  insertAt @0
