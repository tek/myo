module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.Views (Views)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Conc (Lock)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Ribosome (LogReport, Persist, PersistError, Rpc, RpcError, SettingError, Settings, reportStop)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command (Command, ident))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Data.StoreHistory (StoreHistory)
import Myo.Command.Data.UiTarget (UiTarget (UiTarget))
import qualified Myo.Command.Effect.Backend as Backend
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.History (commandOrHistoryByIdent, pushHistory)
import Myo.Command.Proc (waitForShell)
import Myo.Command.RunTask (runTask)
import Myo.Data.CommandId (CommandId (CommandId), commandIdText)
import Myo.Effect.Controller (Controller (CaptureOutput, RunCommand))
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane)

preparePane ::
  Members [Backend !! RunError, AtomicState Views, AtomicState UiState, Stop RunError, Rpc] r =>
  UiTarget ->
  Sem r ()
preparePane (UiTarget ident) =
  mapStop RunError.Render $ mapStop RunError.Toggle do
    openOnePane ident
    restop @RunError Backend.render

type PrepareStack =
  [
    Settings !! SettingError,
    Persist [HistoryEntry] !! PersistError,
    Rpc !! RpcError,
    Lock @@ StoreHistory,
    CommandLog,
    Executions,
    Backend !! RunError,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Input Ident,
    DataLog LogReport,
    ChronosTime,
    Async,
    Log,
    Resource,
    Race
  ]

prepare ::
  Members [Rpc, Stop RunError] r =>
  Members PrepareStack r =>
  RunTask ->
  Sem r ()
prepare = \case
  RunTask _ _ (RunTaskDetails.UiSystem ident) _ _ -> do
    preparePane ident
  RunTask _ Command {ident} (RunTaskDetails.UiShell shellIdent _) _ _ ->
    unlessM (Executions.running shellIdent) do
      Log.debug [exon|Starting inactive shell command `#{commandIdText shellIdent}` async for `#{commandIdText ident}`|]
      void $ async $ reportStop @RunError $ mapStop RunError.Command do
        shellCmd <- commandByIdent "run" shellIdent
        runCommand shellCmd mempty
      waitForShell shellIdent
  RunTask _ _ RunTaskDetails.System _ _ ->
    unit
  RunTask _ _ (RunTaskDetails.Vim _ _) _ _ ->
    unit

-- TODO create effect for history
runCommand ::
  Members PrepareStack r =>
  Members [Rpc, Stop RunError, Stop CommandError] r =>
  Command ->
  ParamValues ->
  Sem r ()
runCommand cmd@Command {ident} params = do
  Log.debug [exon|Running #{show cmd}|]
  task <- runTask cmd params
  CommandLog.archive ident
  prepare task
  resumeHoist RunError.Persist (pushHistory cmd (CommandId task.ident) task.params task.compiled)
  restop (Backend.execute task)
  Log.debug [exon|Command `#{commandIdText ident}` executed successfully|]

captureOutput ::
  Member (Rpc !! RpcError) r =>
  Members [Backend !! RunError, AtomicState CommandState, Stop RunError, Stop CommandError, Input Ident] r =>
  CommandId ->
  Sem r ()
captureOutput ident = do
  cmd <- commandOrHistoryByIdent "capture" ident
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
      mapStop RunError.Command do
        captureOutput ident
