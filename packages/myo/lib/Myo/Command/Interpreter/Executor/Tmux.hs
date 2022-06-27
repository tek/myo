module Myo.Command.Interpreter.Executor.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (quitCopyMode, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (NativeCodecsE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withPanes_, withTmuxApis_)
import qualified Chiasma.View as Views
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Process (Pid)
import Ribosome (HostError, ToErrorMessage, resumeReportError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), ident)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiShell, UiSystem))
import Myo.Command.Data.TmuxTask (TaskType (Kill, Shell, Wait), TmuxTask (TmuxTask), command, pane, taskType)
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions, withExecution)
import qualified Myo.Command.Effect.TmuxMonitor as TmuxMonitor
import Myo.Command.Effect.TmuxMonitor (ScopedTmuxMonitor, TmuxMonitorTask (TmuxMonitorTask), withTmuxMonitor)
import Myo.Data.ProcError (ProcError, unProcError)
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import Myo.Effect.Proc (Proc)
import Myo.Tmux.Proc (panePid, shellBusy, waitForRunningProcess)
import Myo.Ui.Data.UiState (UiState)

acceptCommand ::
  Bool ->
  PaneId ->
  Command ->
  Maybe TmuxTask
acceptCommand shell pane command@Command {kill} =
  Just TmuxTask {..}
  where
    taskType =
      case (shell, kill) of
        (True, _) -> Shell
        (False, True) -> Kill
        (False, False) -> Wait

cmdline :: [Text] -> [Key]
cmdline =
  fmap Lit

paneIdForIdent ::
  Members [AtomicState Views, Stop ViewsError] r =>
  Ident ->
  Sem r PaneId
paneIdForIdent paneIdent =
  stopEither =<< atomicGets (Views.paneId paneIdent)

start ::
  Members [NativeTmux, Stop CodecError, Log] r =>
  Members (NativeCodecsE [TmuxCommand, Panes PaneMode]) r =>
  TmuxTask ->
  Sem r ()
start TmuxTask {pane, command = Command {ident, cmdLines}} =
  withTmuxApis_ @[TmuxCommand, Panes PaneMode] @CodecError do
    quitCopyMode pane
    Log.debug [exon|Sending command `#{identText ident}` to tmux pane #{formatId pane}|]
    sendKeys pane (cmdline cmdLines)

type StartMonitoredStack tmres tme =
  NativeCodecsE [TmuxCommand, Panes PaneMode] ++ [
    ScopedTmuxMonitor tmres !! tme,
    DataLog HostError,
    NativeTmux,
    Resource,
    Log
  ]

startMonitored ::
  ToErrorMessage tme =>
  Member (Stop CodecError) r =>
  Members (StartMonitoredStack tmres tme) r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startMonitored shellPid task@TmuxTask {pane, command = Command {ident}} =
  resumeReportError (Just "tmux") $ withTmuxMonitor TmuxMonitorTask {..} do
    start task
    TmuxMonitor.wait

startTask ::
  ToErrorMessage tme =>
  Member (Stop CodecError) r =>
  Members (StartMonitoredStack tmres tme) r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startTask shellPid = \case
  task@TmuxTask {taskType = Shell} ->
    start task
  task ->
    startMonitored shellPid task

type TmuxRunStack tmres tme =
  StartMonitoredStack tmres tme ++
  NativeCodecsE [Panes Pane, Panes PanePid] ++ [
    Executions,
    CommandLog,
    Proc !! ProcError,
    DataLog HostError,
    AtomicState UiState,
    Log,
    ChronosTime,
    Async,
    Race,
    Embed IO
  ]

waitForCommand ::
  Members [Executions, Log] r =>
  TmuxTask ->
  Sem r ()
waitForCommand (TmuxTask {taskType, command = Command {ident}}) = do
  whenM (Executions.active ident) do
    when (taskType == Kill) do
      Log.debug [exon|Killing running command `#{identText ident}`|]
      Executions.kill ident
    unless (taskType == Shell) do
      Log.debug [exon|Waiting for running command `#{identText ident}`|]
      Executions.wait ident

waitForProcess ::
  Members [Executions, Proc !! ProcError, Log, ChronosTime, Stop RunError] r =>
  Pid ->
  TmuxTask ->
  Sem r ()
waitForProcess shellPid TmuxTask {taskType, command = Command {ident}} = do
  whenM (False <! shellBusy shellPid) do
    when (taskType == Kill) do
      Log.debug [exon|Killing running process to start `#{identText ident}`|]
      Executions.kill ident
    unless (taskType == Shell) do
      Log.debug [exon|Waiting for running process to start `#{identText ident}`|]
      resumeHoist @_ @Proc (RunError.Proc . unProcError) (waitForRunningProcess shellPid)

runInTmux ::
  ∀ tmres tme r .
  ToErrorMessage tme =>
  Members (TmuxRunStack tmres tme) r =>
  Members [NativeTmux, Resource, Stop RunError] r =>
  TmuxTask ->
  Sem r (Maybe [Text])
runInTmux task@TmuxTask {pane, command = Command {ident}} = do
  mapStop RunError.TmuxCodec do
    waitForCommand task
    withExecution ident do
      shellPid <- withPanes_ @PanePid @CodecError (panePid pane)
      startTask shellPid task
      pure Nothing

interpretExecutorTmux ::
  ToErrorMessage tme =>
  Members (TmuxRunStack tmres tme) r =>
  Members [NativeTmux !! TmuxError, AtomicState Views, Resource] r =>
  InterpreterFor (Executor TmuxTask !! RunError) r
interpretExecutorTmux =
  interpretResumable \case
    Executor.Accept (RunTask cmd _ (UiSystem target)) -> do
      paneId <- mapStop RunError.Views (paneIdForIdent target)
      pure (acceptCommand False paneId cmd)
    Executor.Accept (RunTask cmd _ (UiShell _ target)) -> do
      paneId <- mapStop RunError.Views (paneIdForIdent target)
      pure (acceptCommand True paneId cmd)
    Executor.Accept _ ->
      pure Nothing
    Executor.Run task@(TmuxTask _ _ Command {ident}) ->
      runInTmux task !! \ (e :: TmuxError) -> pure (Just [[exon|tmux execution failed for #{identText ident}|], show e])
