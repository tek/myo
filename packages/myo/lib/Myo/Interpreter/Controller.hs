module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Views (Views)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Exon (exon)
import qualified Log
import Log (Severity (Error))
import Polysemy.Chronos (ChronosTime)
import Ribosome (ErrorMessage (ErrorMessage), HostError, Rpc, reportError, reportStop)
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import Myo.Command.Command (commandByIdent)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Data.TmuxTask (TmuxTask)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.RunTask (runTask)
import Myo.Data.ProcessTask (ProcessTask)
import Myo.Effect.Controller (Controller (RunCommand, RunIdent))
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Toggle (ToggleStack, ensurePaneOpen)

handleError ::
  Member (DataLog HostError) r =>
  Command ->
  [Text] ->
  Sem r ()
handleError Command {ident} errors =
  reportError (Just "command") (message errors Error)
  where
    message = \case
      e ->
        ErrorMessage [exon|#{identText ident} failed#{foldMap userMessage (head e)}|]
        (["Controller: command failed", identText ident] <> e)
    userMessage m =
      [exon|: #{m}|]

preparePane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Stop RunError, Rpc] r =>
  Ident ->
  Sem r ()
preparePane ident =
  mapStop RunError.Render $ mapStop RunError.Toggle do
    ensurePaneOpen ident

waitForShell ::
  Members [Executions, ChronosTime] r =>
  Ident ->
  Sem r ()
waitForShell ident =
  Time.while (MilliSeconds 100) do
    not <$> Executions.running ident

type PrepareStack enc dec =
  ToggleStack enc dec ++ [
    Executions,
    Executor ProcessTask !! RunError,
    Executor TmuxTask !! RunError,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
    DataLog HostError,
    Rpc,
    ChronosTime,
    Async,
    Log
  ]

prepare ::
  ∀ enc dec r .
  Member (Stop RunError) r =>
  Members (PrepareStack enc dec) r =>
  RunTask ->
  Sem r ()
prepare = \case
  RunTask _ _ (RunTaskDetails.UiSystem ident) -> do
    preparePane ident
  RunTask _ _ (RunTaskDetails.UiShell shellIdent _) ->
    unlessM (Executions.active shellIdent) do
      Log.debug [exon|Starting inactive shell command `#{identText shellIdent}`|]
      void $ async $ reportStop @RunError (Just "command") do
        runIdent shellIdent
      waitForShell shellIdent
  RunTask _ _ RunTaskDetails.System ->
    unit
  RunTask _ _ RunTaskDetails.Vim ->
    unit

tryExecutor ::
  ∀ task enc dec r .
  Members (ToggleStack enc dec) r =>
  Members [Rpc, Executor task !! RunError, AtomicState UiState, AtomicState Views, Stop RunError] r =>
  RunTask ->
  Sem r (Maybe (Maybe [Text]))
tryExecutor rtask =
  restop @_ @(Executor _) do
    traverse Executor.run =<< Executor.accept rtask

runCommand ::
  Member (Stop RunError) r =>
  Members (PrepareStack enc dec) r =>
  Command ->
  Sem r ()
runCommand cmd = do
  Log.debug [exon|Running command #{show cmd}|]
  task <- mapStop RunError.Command (runTask cmd)
  prepare task
  maybe (stop (RunError.NoRunner task)) (traverse_ (handleError cmd)) =<< runMaybeT do
    MaybeT (tryExecutor @TmuxTask task) <|> MaybeT (tryExecutor @ProcessTask task)

runIdent ::
  Member (Stop RunError) r =>
  Members (PrepareStack enc dec) r =>
  Ident ->
  Sem r ()
runIdent ident =
  mapStop RunError.Command do
    runCommand =<< commandByIdent "run" ident

interpretController ::
  Members (PrepareStack enc dec) r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunIdent ident ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command do
        runIdent ident
    RunCommand cmd ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command do
        runCommand cmd
