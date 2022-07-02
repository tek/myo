module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Views (Views)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Ribosome (HostError, Persist, PersistError, Rpc, SettingError, Settings, reportStop)
import qualified Time
import Time (MilliSeconds (MilliSeconds))

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.LogDir (LogDir)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Data.StoreHistoryLock (StoreHistoryLock)
import qualified Myo.Command.Effect.Backend as Backend
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.History (pushHistory)
import Myo.Command.RunTask (runTask)
import Myo.Effect.Controller (Controller (CaptureOutput, RunCommand, RunIdent))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane)

preparePane ::
  Members [Backend !! RunError, AtomicState Views, AtomicState UiState, Stop RunError, Rpc] r =>
  Ident ->
  Sem r ()
preparePane ident =
  mapStop RunError.Render $ mapStop RunError.Toggle do
    openOnePane ident
    restop @RunError Backend.render

waitForShell ::
  Members [Executions, ChronosTime] r =>
  Ident ->
  Sem r ()
waitForShell ident =
  Time.while (MilliSeconds 100) do
    not <$> Executions.running ident

type PrepareStack =
  [
    Settings !! SettingError,
    Persist [HistoryEntry] !! PersistError,
    Sync StoreHistoryLock,
    Executions,
    Backend !! RunError,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
    DataLog HostError,
    Rpc,
    ChronosTime,
    Async,
    Log,
    Resource
  ]

prepare ::
  Member (Stop RunError) r =>
  Members PrepareStack r =>
  RunTask ->
  Sem r ()
prepare = \case
  RunTask _ (RunTaskDetails.UiSystem ident) -> do
    preparePane ident
  RunTask _ (RunTaskDetails.UiShell shellIdent _) ->
    unlessM (Executions.active shellIdent) do
      Log.debug [exon|Starting inactive shell command `#{identText shellIdent}`|]
      void $ async $ reportStop @RunError (Just "command") do
        mapStop RunError.Command (runIdent shellIdent)
      waitForShell shellIdent
  RunTask _ RunTaskDetails.System ->
    unit
  RunTask _ RunTaskDetails.Vim ->
    unit

-- TODO create effect for history
runCommand ::
  Members PrepareStack r =>
  Members [Stop RunError, Stop CommandError] r =>
  Command ->
  Sem r ()
runCommand cmd = do
  Log.debug [exon|Running command #{show cmd}|]
  task <- runTask cmd
  prepare task
  resumeHoist RunError.Persist (pushHistory cmd)
  restop (Backend.execute task)

runIdent ::
  Members PrepareStack r =>
  Members [Settings !! SettingError, Stop RunError, Stop CommandError, Resource] r =>
  Ident ->
  Sem r ()
runIdent ident =
  runCommand =<< commandByIdent "run" ident

captureOutput ::
  Members [Backend !! RunError, Reader LogDir, AtomicState CommandState, Stop RunError, Stop CommandError] r =>
  Ident ->
  Sem r ()
captureOutput ident = do
  cmd <- commandByIdent "run" ident
  task <- runTask cmd
  restop (Backend.captureOutput task)

interpretController ::
  Members PrepareStack r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunIdent ident ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command do
        runIdent ident
    RunCommand cmd ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod $ mapStop RunError.Command do
        runCommand cmd
    CaptureOutput ident ->
      mapStop RunError.Command do
        captureOutput ident
