module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Ribosome (ReportLog, Rpc, RpcError, reportStop)

import Myo.Command.Data.Command (Command (Command, ident))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Data.UiTarget (UiTarget (UiTarget))
import qualified Myo.Command.Effect.Backend as Backend
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Proc (waitForShell)
import Myo.Command.RunTask (runTask)
import Myo.Data.CommandId (CommandId (CommandId), commandIdText)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller (CaptureOutput, RunCommand))
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)
import Myo.Interpreter.Commands (interpretCommands, interpretCommandsNoHistory)
import Myo.Interpreter.History (interpretHistoryTransient)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane)

preparePane ::
  Members [Backend !! RunError, AtomicState UiState, Stop RunError] r =>
  UiTarget ->
  Sem r ()
preparePane (UiTarget ident) =
  mapStop RunError.Render $ mapStop RunError.Toggle do
    openOnePane ident
    restop @RunError Backend.render

type PrepareStack =
  [
    Rpc !! RpcError,
    CommandLog,
    Executions,
    Backend !! RunError,
    Commands !! CommandError,
    History !! RunError,
    AtomicState UiState,
    Input Ident,
    ReportLog,
    ChronosTime,
    Async,
    Log,
    Resource,
    Race
  ]

-- | Run a shell async because Backend potentially blocks to read output.
runShellAsync ::
  Members PrepareStack r =>
  Command ->
  Sem r ()
runShellAsync shellCmd =
  void $ async $ reportStop @RunError do
    runCommand shellCmd mempty

prepare ::
  Member (Stop RunError) r =>
  Members PrepareStack r =>
  RunTask ->
  Sem r ()
prepare = \case
  RunTask {details = RunTaskDetails.UiSystem ident} ->
    preparePane ident
  RunTask {command = Command {ident}, details = RunTaskDetails.UiShell shellIdent _} ->
    unlessM (Executions.running shellIdent) do
      Log.debug [exon|Starting inactive shell command '#{commandIdText shellIdent}' async for '#{commandIdText ident}'|]
      shellCmd <- resumeHoist RunError.Command (Commands.queryId shellIdent)
      runShellAsync shellCmd
      waitForShell shellIdent
  RunTask {details = RunTaskDetails.System} ->
    unit
  RunTask {details = RunTaskDetails.Vim _ _} ->
    unit

runCommand ::
  Members PrepareStack r =>
  Member (Stop RunError) r =>
  Command ->
  ParamValues ->
  Sem r ()
runCommand cmd@Command {ident} params = do
  Log.debug [exon|Running #{show cmd}|]
  task <- resumeHoist RunError.Command (runTask cmd params)
  CommandLog.archive ident
  prepare task
  restop (History.push cmd (CommandId task.ident) task.params task.compiled)
  restop (Backend.execute task)
  Log.debug [exon|Command '#{commandIdText ident}' executed successfully|]

captureOutput ::
  Members [Rpc !! RpcError, History, Commands] r =>
  Members [Backend !! RunError, Stop RunError, Stop CommandError, Input Ident] r =>
  CommandId ->
  Sem r ()
captureOutput ident = do
  cmd <- Commands.queryIdBoth ident
  task <- runTask cmd mempty
  restop (Backend.captureOutput task)

handleErrors ::
  Members [Rpc !! RpcError, Stop RunError] r =>
  InterpretersFor [Rpc, Stop CommandError, Stop TreeModError, Stop RenderError, Stop ToggleError] r
handleErrors =
  mapStop RunError.Toggle .
  mapStop RunError.Render .
  mapStop RunError.TreeMod .
  mapStop RunError.Command .
  resumeHoist RunError.Rpc

interpretController ::
  Members PrepareStack r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunCommand cmd params ->
      handleErrors do
        runCommand cmd params
    CaptureOutput ident ->
      mapStop RunError.Command $ restop @RunError @History $ restop @CommandError @Commands do
        captureOutput ident

interpretControllerTransient ::
  Members [CommandLog, Backend !! RunError, AtomicState UiState, Input Ident] r =>
  Members [Rpc !! RpcError, ReportLog, ChronosTime, Executions, Log, Resource, Async, Race, Embed IO] r =>
  [HistoryEntry] ->
  InterpretersFor [
    Controller !! RunError,
    Commands !! CommandError,
    History !! RunError
  ] r
interpretControllerTransient history =
  interpretHistoryTransient history .
  interpretCommands .
  interpretController

interpretControllerNoHistory ::
  Members [CommandLog, Backend !! RunError, AtomicState UiState, Input Ident] r =>
  Members [Rpc !! RpcError, ReportLog, ChronosTime, Executions, Log, Resource, Async, Race, Embed IO] r =>
  InterpretersFor [
    Controller !! RunError,
    Commands !! CommandError,
    History !! RunError
  ] r
interpretControllerNoHistory =
  interpretCommandsNoHistory .
  interpretController
