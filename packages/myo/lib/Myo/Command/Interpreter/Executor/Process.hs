module Myo.Command.Interpreter.Executor.Process where

import Chiasma.Data.Ident (Ident)
import Conc (PScoped)
import qualified Data.List as List
import qualified Polysemy.Process as Process
import Polysemy.Process (
  OutputPipe (Stderr, Stdout),
  Process,
  ProcessError (Exit, Unknown),
  SysProcConf,
  interpretProcessInputText,
  interpretProcessNative,
  interpretProcessOutputLeft,
  interpretProcessOutputRight,
  interpretProcessOutputTextLines,
  withProcess,
  )
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process.Typed (proc)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command, cmdLines))
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunEvent as RunEvent
import Myo.Command.Data.RunEvent (RunEvent (RunEvent))
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (System, UiSystem))
import Myo.Command.Effect.Executor (Executor)
import Myo.Command.Interpreter.Executor.Generic (captureUnsupported, interceptExecutor)
import Myo.Data.ProcessTask (ProcessTask (ProcessTask))

outputEvent ::
  Members [Events eres RunEvent, State [Text]] r =>
  Ident ->
  Either Text Text ->
  Sem r ()
outputEvent ident = \case
  Right out -> publish (RunEvent ident (RunEvent.Output out))
  Left out -> modify' (out :)

runProcess ::
  ∀ eres pres r .
  Member (Stop RunError) r =>
  Members [Events eres RunEvent, PScoped (String, [String]) pres (Process Text (Either Text Text)) !! ProcessError] r =>
  ProcessTask ->
  Sem r ()
runProcess (ProcessTask ident cmd) =
  evalState mempty $ resuming checkError $ withProcess cmd do
    forever do
      outputEvent ident =<< Process.recv
  where
    checkError = \case
      Unknown _ -> failure
      Exit ExitSuccess -> unit
      Exit (ExitFailure _) -> failure
    failure =
      stop . RunError.SubprocFailed =<< get

conf :: (String, [String]) -> Sem r SysProcConf
conf (exe, args) =
  pure (proc exe args)

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
  RunTask cmd (UiSystem _) ->
    pure (processTask cmd)
  _ ->
    pure Nothing

interpretExecutorProcess ::
  ∀ eres pres r a .
  Member (Executor !! RunError) r =>
  Members [Events eres RunEvent, PScoped (String, [String]) pres (Process Text (Either Text Text)) !! ProcessError] r =>
  Sem r a ->
  Sem r a
interpretExecutorProcess =
  interceptExecutor acceptCommand runProcess (captureUnsupported "process")

interpretExecutorProcessNative ::
  Members [Executor !! RunError, Events res RunEvent, Resource, Race, Async, Embed IO] r =>
  Sem r a ->
  Sem r a
interpretExecutorProcessNative =
  interpretProcessOutputTextLines @'Stderr .
  interpretProcessOutputLeft @'Stderr .
  interpretProcessOutputTextLines @'Stdout .
  interpretProcessOutputRight @'Stdout .
  interpretProcessInputText .
  interpretProcessNative def conf .
  interpretExecutorProcess .
  insertAt @0
