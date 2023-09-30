module Myo.Command.Interpreter.Backend.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (capturePane, quitCopyMode, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (NativeCodecsE, NativeCommandCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withPanes_, withTmuxApis_, withTmux_)
import qualified Chiasma.View as Views
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Process (Pid)
import Ribosome (LogReport, Reportable, Rpc, RpcError, resumeLogReport)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), commandShell, ident)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiShell, UiSystem))
import Myo.Command.Data.SocatExe (SocatExe)
import qualified Myo.Command.Data.TmuxTask
import Myo.Command.Data.TmuxTask (TaskType (Kill, Shell, Wait), TmuxTask (TmuxTask), command, pane, target, taskType)
import Myo.Command.Data.UiTarget (UiTarget (UiTarget))
import Myo.Command.Effect.Backend (Backend)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions, withExecution)
import Myo.Command.Effect.SocketReader (ScopedSocketReader)
import qualified Myo.Command.Effect.TmuxMonitor as TmuxMonitor
import Myo.Command.Effect.TmuxMonitor (ScopedTmuxMonitor, TmuxMonitorTask (TmuxMonitorTask), withTmuxMonitor)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)
import Myo.Command.Interpreter.TmuxMonitor (interpretTmuxMonitor, interpretTmuxMonitorNoLog)
import Myo.Command.Proc (waitForShell)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Data.ProcError (ProcError, unProcError)
import Myo.Effect.Proc (Proc)
import Myo.Tmux.Proc (leafPid, panePid, shellBusy, waitForRunningProcess)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Render (renderTmux)

acceptCommand ::
  Bool ->
  UiTarget ->
  PaneId ->
  Command ->
  [Text] ->
  Maybe TmuxTask
acceptCommand shell target pane command@Command {kill} compiled =
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
  UiTarget ->
  Sem r PaneId
paneIdForIdent (UiTarget paneIdent) =
  stopEither =<< atomicGets (Views.paneId paneIdent)

start ::
  Members [NativeTmux, Stop CodecError, Log] r =>
  Members (NativeCodecsE [TmuxCommand, Panes PaneMode]) r =>
  TmuxTask ->
  Sem r ()
start TmuxTask {pane, command = Command {ident}, compiled} =
  withTmuxApis_ @[TmuxCommand, Panes PaneMode] @CodecError do
    quitCopyMode pane
    Log.debug [exon|Sending command `#{commandIdText ident}` to tmux pane #{formatId pane}|]
    sendKeys pane (cmdline compiled)

type StartMonitoredStack =
  NativeCodecsE [TmuxCommand, Panes PaneMode] ++ [
    DataLog LogReport,
    Log
  ]

startMonitored ::
  Reportable tme =>
  Members [ScopedTmuxMonitor !! tme, NativeTmux, Stop CodecError] r =>
  Members StartMonitoredStack r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startMonitored shellPid task@TmuxTask {pane, command = Command {ident}} =
  resumeLogReport $ withTmuxMonitor TmuxMonitorTask {..} do
    start task
    TmuxMonitor.wait

startTask ::
  Reportable tme =>
  Members [ScopedTmuxMonitor !! tme, NativeTmux, Stop CodecError] r =>
  Members StartMonitoredStack r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startTask shellPid = \case
  task@TmuxTask {taskType = Shell} ->
    start task
  task ->
    startMonitored shellPid task

waitForCommand ::
  Members [Executions, Log] r =>
  TmuxTask ->
  Sem r ()
waitForCommand (TmuxTask {taskType, target}) =
  Executions.activeTarget target >>= traverse_ \ ident -> do
    when (taskType == Kill) do
      Log.info [exon|Killing running command `#{commandIdText ident}`|]
      Executions.terminate ident
    unless (taskType == Shell) do
      Log.info [exon|Waiting for running command `#{commandIdText ident}`|]
      Executions.wait ident

-- TODO this isn't used
waitForProcess ::
  Members [Executions, Proc !! ProcError, Log, ChronosTime, Stop RunError] r =>
  Pid ->
  TmuxTask ->
  Sem r ()
waitForProcess shellPid TmuxTask {taskType, command = Command {ident}} = do
  whenM (False <! shellBusy shellPid) do
    when (taskType == Kill) do
      Log.info [exon|Killing running process to start `#{commandIdText ident}`|]
      Executions.terminate ident
    unless (taskType == Shell) do
      Log.info [exon|Waiting for running process to start `#{commandIdText ident}`|]
      resumeHoist @_ @Proc (RunError.Proc . (.unProcError)) (waitForRunningProcess shellPid)

activeShellPid ::
  Members [Proc !! ProcError, Executions, ChronosTime, Stop RunError, Race] r =>
  CommandId ->
  Sem r Pid
activeShellPid i = do
  waitForShell i
  mainPid <- stopNote (RunError.Proc "Command shell vanished") =<< Executions.pid i
  resumeHoist @_ @Proc (RunError.Proc . (.unProcError)) (leafPid mainPid)

type TmuxRunStack =
  [
    Proc !! ProcError,
    Executions,
    NativeTmux !! TmuxError,
    ChronosTime,
    Race
  ] ++
  StartMonitoredStack ++
  NativeCodecsE [Panes Pane, Panes PanePid]

runInTmux ::
  ∀ tme r .
  Reportable tme =>
  Members TmuxRunStack r =>
  Members [ScopedTmuxMonitor !! tme, Resource, Stop RunError] r =>
  TmuxTask ->
  Sem r ()
runInTmux task@TmuxTask {pane, target, command = Command {ident, commandShell}} =
  resumeHoist RunError.Tmux $ mapStop RunError.TmuxCodec do
    waitForCommand task
    withExecution ident commandShell (Just target) \ shell -> do
      shellPid <- maybe (withPanes_ @PanePid @CodecError (panePid pane)) activeShellPid shell
      startTask shellPid task

acceptTmux ::
  Members [AtomicState Views, Stop RunError] r =>
  RunTask ->
  Sem r (Maybe TmuxTask)
acceptTmux = \case
  RunTask _ cmd (UiSystem target) compiled _ -> do
    paneId <- mapStop RunError.Views (paneIdForIdent target)
    pure (acceptCommand False target paneId cmd compiled)
  RunTask _ cmd (UiShell _ target) compiled _ -> do
    paneId <- mapStop RunError.Views (paneIdForIdent target)
    pure (acceptCommand True target paneId cmd compiled)
  _ ->
    pure Nothing

captureOutput ::
  Members [NativeTmux !! TmuxError, CommandLog, NativeCommandCodecE, Stop RunError] r =>
  TmuxTask ->
  Sem r ()
captureOutput (TmuxTask {pane, command = Command {ident}}) =
  resumeHoist @_ @NativeTmux RunError.Tmux $ mapStop RunError.TmuxCodec do
    text <- withTmux_ (capturePane pane)
    CommandLog.set ident (Text.unlines text)

render ::
  Members (NativeCodecsE [TmuxCommand, Panes Pane]) r =>
  Members [AtomicState Views, AtomicState UiState, NativeTmux !! TmuxError, Rpc !! RpcError, Stop RunError] r =>
  Sem r ()
render =
  resumeHoist RunError.Rpc $ mapStop RunError.Render $ resumeHoist @_ @NativeTmux RunError.Tmux do
    renderTmux

-- TODO if multiple executors accept ui tasks, stopping when the pane isn't part of the state might be wrong.
-- but if all uis share the same state, it's correct but should probably done before this
-- but given that it returns a tmux-specific PaneId, they should probably be separate
interpretBackendTmuxWithLog ::
  Reportable sre =>
  Members TmuxRunStack r =>
  Members [AtomicState UiState, ChronosTime, CommandLog, Rpc !! RpcError, Reader (Maybe SocatExe)] r =>
  Members [Backend !! RunError, ScopedSocketReader !! sre, Race, AtomicState Views, Resource] r =>
  Sem r a ->
  Sem r a
interpretBackendTmuxWithLog =
  interpretTmuxMonitor .
  interceptBackend acceptTmux runInTmux captureOutput render .
  raise

interpretBackendTmuxNoLog ::
  Members TmuxRunStack r =>
  Members [AtomicState UiState, Rpc !! RpcError] r =>
  Members [Backend !! RunError, AtomicState Views, ChronosTime, Race, Resource] r =>
  Sem r a ->
  Sem r a
interpretBackendTmuxNoLog =
  interpretTmuxMonitorNoLog .
  interceptBackend acceptTmux runInTmux (captureUnsupported "tmux-nolog") render .
  raise
