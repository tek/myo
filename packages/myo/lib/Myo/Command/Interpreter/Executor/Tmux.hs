module Myo.Command.Interpreter.Executor.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (capturePane, quitCopyMode, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
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
import Ribosome (HostError, ToErrorMessage, resumeReportError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), ident)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiShell, UiSystem))
import Myo.Command.Data.TmuxTask (TaskType (Kill, Shell, Wait), TmuxTask (TmuxTask), command, pane, taskType)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions, withExecution)
import Myo.Command.Effect.Executor (Executor)
import Myo.Command.Effect.SocketReader (ScopedSocketReader)
import qualified Myo.Command.Effect.TmuxMonitor as TmuxMonitor
import Myo.Command.Effect.TmuxMonitor (ScopedTmuxMonitor, TmuxMonitorTask (TmuxMonitorTask), withTmuxMonitor)
import Myo.Command.Interpreter.Executor.Generic (captureUnsupported, interceptExecutor)
import Myo.Command.Interpreter.TmuxMonitor (interpretTmuxMonitor, interpretTmuxMonitorNoLog)
import Myo.Data.ProcError (ProcError, unProcError)
import Myo.Effect.Proc (Proc)
import Myo.Tmux.Proc (panePid, shellBusy, waitForRunningProcess)

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

type StartMonitoredStack =
  NativeCodecsE [TmuxCommand, Panes PaneMode] ++ [
    DataLog HostError,
    Log
  ]

startMonitored ::
  ToErrorMessage tme =>
  Members [ScopedTmuxMonitor tmres !! tme, NativeTmux, Stop CodecError] r =>
  Members StartMonitoredStack r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startMonitored shellPid task@TmuxTask {pane, command = Command {ident}} =
  resumeReportError (Just "tmux") $ withTmuxMonitor TmuxMonitorTask {..} do
    start task
    TmuxMonitor.wait

startTask ::
  ToErrorMessage tme =>
  Members [ScopedTmuxMonitor tmres !! tme, NativeTmux, Stop CodecError] r =>
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

type TmuxRunStack =
  [
    Executions,
    NativeTmux !! TmuxError
  ] ++
  StartMonitoredStack ++
  NativeCodecsE [Panes Pane, Panes PanePid]

runInTmux ::
  ∀ tmres tme r .
  ToErrorMessage tme =>
  Members TmuxRunStack r =>
  Members [ScopedTmuxMonitor tmres !! tme, Resource, Stop RunError] r =>
  TmuxTask ->
  Sem r ()
runInTmux task@TmuxTask {pane, command = Command {ident}} = do
  resumeHoist RunError.Tmux $ mapStop RunError.TmuxCodec do
    waitForCommand task
    withExecution ident do
      shellPid <- withPanes_ @PanePid @CodecError (panePid pane)
      startTask shellPid task

acceptTmux ::
  Members [AtomicState Views, Stop RunError] r =>
  RunTask ->
  Sem r (Maybe TmuxTask)
acceptTmux = \case
  RunTask cmd (UiSystem target) -> do
    paneId <- mapStop RunError.Views (paneIdForIdent target)
    pure (acceptCommand False paneId cmd)
  RunTask cmd (UiShell _ target) -> do
    paneId <- mapStop RunError.Views (paneIdForIdent target)
    pure (acceptCommand True paneId cmd)
  _ ->
    pure Nothing

captureOutput ::
  Members [NativeTmux !! TmuxError, CommandLog, NativeCommandCodecE, Stop RunError] r =>
  TmuxTask ->
  Sem r ()
captureOutput (TmuxTask _ pane Command {ident}) =
  resumeHoist @_ @NativeTmux RunError.Tmux $ mapStop RunError.TmuxCodec do
    text <- withTmux_ (capturePane pane)
    CommandLog.set ident (Text.unlines text)

-- TODO if multiple executors accept ui tasks, stopping when the pane isn't part of the state might be wrong.
-- but if all uis share the same state, it's correct but should probably done before this
-- but given that it returns a tmux-specific PaneId, they should probably be separate
interpretExecutorTmuxWithLog ::
  ToErrorMessage sre =>
  Members TmuxRunStack r =>
  Members [Proc !! ProcError, ChronosTime, CommandLog] r =>
  Members [Executor !! RunError, ScopedSocketReader socket !! sre, Race, AtomicState Views, Resource] r =>
  Sem r a ->
  Sem r a
interpretExecutorTmuxWithLog =
  interpretTmuxMonitor .
  interceptExecutor acceptTmux runInTmux captureOutput .
  raise

interpretExecutorTmuxNoLog ::
  Members TmuxRunStack r =>
  Members [Proc !! ProcError, Executor !! RunError, AtomicState Views, ChronosTime, Race, Resource] r =>
  Sem r a ->
  Sem r a
interpretExecutorTmuxNoLog =
  interpretTmuxMonitorNoLog .
  interceptExecutor acceptTmux runInTmux (captureUnsupported "tmux-nolog") .
  raise
