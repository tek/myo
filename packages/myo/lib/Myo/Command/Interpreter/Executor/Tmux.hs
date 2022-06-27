module Myo.Command.Interpreter.Executor.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (quitCopyMode, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.PipePaneParams (target)
import Chiasma.Data.SendKeysParams (Key (Lit))
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (PipePane))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (NativeCodecE, NativeCodecsE, NativeCommandCodecE)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withPanes_, withTmuxApis_, withTmux_)
import qualified Chiasma.View as Views
import Conc (race_)
import Control.Lens.Regex.ByteString (match, regex)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Process (Pid)
import Ribosome (HostError, ToErrorMessage, resumeReportError)
import Time (MilliSeconds (MilliSeconds), while)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), ident)
import qualified Myo.Command.Data.ExecutionState as ExecutionState
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiShell, UiSystem))
import Myo.Command.Data.TmuxTask (TaskType (Kill, Shell, Wait), TmuxTask (TmuxTask), command, pane, taskType)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Command.Effect.SocketReader as SocketReader
import Myo.Command.Effect.SocketReader (ScopedSocketReader, SocketReader, socketReader)
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.ProcError (ProcError, unProcError)
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Loop (useWhileJust)
import Myo.Tmux.Proc (commandPid, panePid, shellBusy, waitForRunningProcess)
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

pollPid ::
  Members [Executions, Proc !! ProcError, ChronosTime] r =>
  Ident ->
  Pid ->
  Sem r ()
pollPid ident shellPid = do
  resumeAs @_ @Proc Nothing (commandPid shellPid) >>= traverse_ \ pid -> do
    Executions.setState ident (ExecutionState.Tracked pid)
    while (MilliSeconds 500) do
      False <! Proc.exists pid

sanitizeOutput :: ByteString -> ByteString
sanitizeOutput =
  [regex|\r\n|] . match .~ "\n"

readOutput ::
  Members [SocketReader, CommandLog] r =>
  Ident ->
  Sem r ()
readOutput ident =
  useWhileJust SocketReader.chunk (CommandLog.append ident . sanitizeOutput)

type MonitorStack =
  [
    CommandLog,
    Proc !! ProcError,
    Executions,
    NativeTmux,
    NativeCodecE (Panes PanePid),
    DataLog HostError,
    ChronosTime,
    Race,
    Log,
    Embed IO
  ]

monitor ::
  Members MonitorStack r =>
  Member SocketReader r =>
  Ident ->
  PaneId ->
  Pid ->
  Sem r ()
monitor ident paneId shellPid = do
  Log.debug [exon|Monitoring tmux command `#{identText ident}` in #{formatId paneId}|]
  race_ (Executions.waitKill ident) (race_ (pollPid ident shellPid) (readOutput ident))

start ::
  Members (NativeCodecsE [TmuxCommand, Panes PaneMode]) r =>
  Members [NativeTmux, Stop CodecError, Log] r =>
  TmuxTask ->
  Sem r ()
start TmuxTask {pane, command = Command {ident, cmdLines}} =
  withTmuxApis_ @[TmuxCommand, Panes PaneMode] @CodecError do
    quitCopyMode pane
    Log.debug [exon|Sending command `#{identText ident}` to tmux pane #{formatId pane}|]
    sendKeys pane (cmdline cmdLines)

pipingToSocket ::
  Members [NativeTmux, SocketReader, NativeCommandCodecE, Stop CodecError, Resource] r =>
  PaneId ->
  Sem r a ->
  Sem r a
pipingToSocket pane ma =
  bracket_ acquire release ma
  where
    acquire = do
      socket <- SocketReader.path
      withTmux_ do
        pipePaneToSocket pane socket
    release =
      withTmux_ do
        Tmux.send (PipePane def { target = Target.Pane pane })

type StartMonitoredStack socket sre =
  MonitorStack ++ NativeCodecsE [TmuxCommand, Panes PaneMode] ++ [
    ScopedSocketReader socket !! sre,
    Stop CodecError,
    Resource
  ]

startMonitored ::
  ToErrorMessage sre =>
  Members (StartMonitoredStack socket sre) r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startMonitored shellPid task@TmuxTask {pane, command = Command {ident}} =
  resumeReportError (Just "tmux") $ socketReader ident $ pipingToSocket pane do
    start task
    monitor ident pane shellPid

startTask ::
  ToErrorMessage sre =>
  Members (StartMonitoredStack socket sre) r =>
  Pid ->
  TmuxTask ->
  Sem r ()
startTask shellPid = \case
  task@TmuxTask {taskType = Shell} ->
    start task
  task ->
    startMonitored shellPid task

type TmuxRunStack socket sre =
  NativeCodecsE [TmuxCommand, Panes Pane, Panes PaneMode, Panes PanePid] ++ [
    ScopedSocketReader socket !! sre,
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

prepare ::
  Member Executions r =>
  TmuxTask ->
  Sem r ()
prepare (TmuxTask {command = Command {ident}}) =
  Executions.start ident

finalizeState ::
  Member Executions r =>
  Ident ->
  Sem r ()
finalizeState ident =
  Executions.stop ident

runInTmux ::
  ∀ socket sre r .
  ToErrorMessage sre =>
  Members (TmuxRunStack socket sre) r =>
  Members [NativeTmux, Resource, Stop RunError] r =>
  TmuxTask ->
  Sem r (Maybe [Text])
runInTmux task@TmuxTask {pane, command = Command {ident}} = do
  mapStop RunError.TmuxCodec do
    waitForCommand task
    bracket_ (prepare task) (finalizeState ident) do
      shellPid <- withPanes_ @PanePid @CodecError (panePid pane)
      startTask shellPid task
      pure Nothing

interpretExecutorTmux ::
  ToErrorMessage sre =>
  Members (TmuxRunStack socket sre) r =>
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
