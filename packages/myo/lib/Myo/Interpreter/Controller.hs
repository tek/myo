module Myo.Interpreter.Controller where

import Chiasma.Data.Views (Views)
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
import Myo.Command.Data.LogDir (LogDir)
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
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Effect.Controller (Controller (CaptureOutput, RunCommand, RunIdent))
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
    Lock @@ StoreHistory,
    CommandLog,
    Executions,
    Backend !! RunError,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
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
  RunTask _ (RunTaskDetails.UiSystem ident) -> do
    preparePane ident
  RunTask Command {ident} (RunTaskDetails.UiShell shellIdent _) ->
    unlessM (Executions.running shellIdent) do
      Log.debug [exon|Starting inactive shell command `#{commandIdText shellIdent}` async for `#{commandIdText ident}`|]
      void $ async $ reportStop @RunError do
        mapStop RunError.Command (runIdent shellIdent)
      waitForShell shellIdent
  RunTask _ RunTaskDetails.System ->
    unit
  RunTask _ (RunTaskDetails.Vim _ _) ->
    unit

-- TODO create effect for history
runCommand ::
  Members PrepareStack r =>
  Members [Rpc, Stop RunError, Stop CommandError] r =>
  Command ->
  Sem r ()
runCommand cmd@Command {ident} = do
  Log.debug [exon|Running #{show cmd}|]
  task <- runTask cmd
  CommandLog.archive ident
  prepare task
  resumeHoist RunError.Persist (pushHistory cmd)
  restop (Backend.execute task)
  Log.debug [exon|Command `#{commandIdText ident}` executed successfully|]

runIdent ::
  Members PrepareStack r =>
  Members [Settings !! SettingError, Rpc, Stop RunError, Stop CommandError, Resource] r =>
  CommandId ->
  Sem r ()
runIdent ident =
  runCommand =<< commandByIdent "run" ident

captureOutput ::
  Members [Backend !! RunError, Reader LogDir, AtomicState CommandState, Stop RunError, Stop CommandError] r =>
  CommandId ->
  Sem r ()
captureOutput ident = do
  cmd <- commandOrHistoryByIdent "capture" ident
  task <- runTask cmd
  restop (Backend.captureOutput task)

interpretController ::
  Members PrepareStack r =>
  Member (Rpc !! RpcError) r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunIdent ident ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command $ resumeHoist RunError.Rpc do
        runIdent ident
    RunCommand cmd ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command $ resumeHoist RunError.Rpc do
        runCommand cmd
    CaptureOutput ident ->
      mapStop RunError.Command do
        captureOutput ident
