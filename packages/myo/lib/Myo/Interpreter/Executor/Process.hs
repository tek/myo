module Myo.Interpreter.Executor.Process where

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
import qualified Myo.Command.Data.RunEvent as RunEvent
import Myo.Command.Data.RunEvent (RunEvent (RunEvent))
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (System, UiSystem))
import Myo.Data.ProcessTask (ProcessTask (ProcessTask))
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)

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
  Members [Events eres RunEvent, PScoped (String, [String]) pres (Process Text (Either Text Text)) !! ProcessError] r =>
  ProcessTask ->
  Sem r (Maybe [Text])
runProcess (ProcessTask ident cmd) =
  evalState mempty $ resuming checkError $ withProcess cmd do
    forever do
      outputEvent ident =<< Process.recv
  where
    checkError = \case
      Unknown _ -> Just <$> get
      Exit ExitSuccess -> pure Nothing
      Exit (ExitFailure _) -> Just <$> get

conf :: (String, [String]) -> Sem r SysProcConf
conf (exe, args) =
  pure (proc exe args)

acceptCommand :: Command -> Maybe ProcessTask
acceptCommand = \case
  Command {ident, cmdLines = [l]} ->
    case List.words (toString l) of
      (h : t) -> Just (ProcessTask ident (h, t))
      [] -> Nothing
  _ ->
    Nothing

interpretExecutorProcess ::
  ∀ eres pres r .
  Members [Events eres RunEvent, PScoped (String, [String]) pres (Process Text (Either Text Text)) !! ProcessError] r =>
  InterpreterFor (Executor ProcessTask) r
interpretExecutorProcess =
  interpret \case
    Executor.Accept (RunTask cmd _ System) ->
      pure (acceptCommand cmd)
    Executor.Accept (RunTask cmd _ (UiSystem _)) ->
      pure (acceptCommand cmd)
    Executor.Accept _ ->
      pure Nothing
    Executor.Run cmd ->
      runProcess cmd

interpretExecutorProcessNative ::
  Members [Events res RunEvent, Resource, Race, Async, Embed IO] r =>
  InterpreterFor (Executor ProcessTask) r
interpretExecutorProcessNative =
  interpretProcessOutputTextLines @'Stderr .
  interpretProcessOutputLeft @'Stderr .
  interpretProcessOutputTextLines @'Stdout .
  interpretProcessOutputRight @'Stdout .
  interpretProcessInputText .
  interpretProcessNative def conf .
  insertAt @1 .
  interpretExecutorProcess .
  raiseUnder
