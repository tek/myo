module Myo.Command.Interpreter.TmuxMonitor where

import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.PipePaneParams (target)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (PipePane))
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Conc (PScoped, interpretPScopedResumableWith, interpretPScopedResumable_, race_)
import Control.Lens.Regex.ByteString (match, regex)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Process (Pid)
import Time (MilliSeconds (MilliSeconds), while)

import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.TmuxMonitorError (TmuxMonitorError (SocketReader, View))
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Command.Effect.SocketReader as SocketReader
import Myo.Command.Effect.SocketReader (ScopedSocketReader, SocketReader, socketReader)
import Myo.Command.Effect.TmuxMonitor (TmuxMonitor (Wait), TmuxMonitorTask (TmuxMonitorTask), ident, pane, shellPid)
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.ProcError (ProcError)
import Myo.Data.ViewError (ViewError (TmuxApi, TmuxCodec))
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Loop (useWhileJust)
import Myo.Tmux.Proc (commandPid)

type ScopeEffects =
  '[
    SocketReader
  ]

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
  ([regex|\r\n|] . match .~ "\n") . ([regex|(\x{9b}|\x{1b}\[)[0-?]*[ -\/]*[@-~]|] . match .~ "")

readOutput ::
  Members [SocketReader, CommandLog] r =>
  Ident ->
  Sem r ()
readOutput ident =
  useWhileJust SocketReader.chunk (CommandLog.append ident . sanitizeOutput)

waitBasic ::
  Members [Executions, Proc !! ProcError, ChronosTime, Race] r =>
  Ident ->
  Pid ->
  Sem r ()
waitBasic ident shellPid =
  race_ (pollPid ident shellPid) (Executions.waitKill ident)

pipingToSocket ::
  Members [NativeTmux !! TmuxError, SocketReader, NativeCommandCodecE, Stop ViewError, Resource] r =>
  PaneId ->
  Sem r a ->
  Sem r a
pipingToSocket pane =
  resumeHoist TmuxApi . mapStop TmuxCodec . bracket_ acquire release . insertAt @0
  where
    acquire = do
      socket <- SocketReader.path
      withTmux_ do
        pipePaneToSocket pane socket
    release =
      withTmux_ do
        Tmux.send (PipePane def { target = Target.Pane pane })

startLog ::
  Member Log r =>
  Ident ->
  PaneId ->
  Sem r ()
startLog ident pane =
  Log.debug [exon|Monitoring tmux command `#{identText ident}` in #{formatId pane}|]

withLog ::
  ∀ socket sre r a .
  Members [NativeTmux !! TmuxError, ScopedSocketReader socket !! sre, NativeCommandCodecE, Resource] r =>
  TmuxMonitorTask ->
  (TmuxMonitorTask -> Sem (SocketReader : Stop (TmuxMonitorError sre) : r) a) ->
  Sem (Stop (TmuxMonitorError sre) : r) a
withLog task@TmuxMonitorTask {..} use =
  resumeHoist SocketReader $ mapStop View do
    socketReader ident $ pipingToSocket pane do
      insertAt @1 (use task)

interpretTmuxMonitor ::
  Members [NativeTmux !! TmuxError, Proc !! ProcError, ChronosTime, CommandLog] r =>
  Members [Executions, ScopedSocketReader socket !! sre, NativeCommandCodecE, Resource, Log, Race] r =>
  InterpreterFor (PScoped TmuxMonitorTask TmuxMonitorTask TmuxMonitor !! TmuxMonitorError sre) r
interpretTmuxMonitor =
  interpretPScopedResumableWith @ScopeEffects withLog \ TmuxMonitorTask {..} -> \case
    Wait -> do
      startLog ident pane
      race_ (readOutput ident) (waitBasic ident shellPid)

interpretTmuxMonitorNoLog ::
  Members [Proc !! ProcError, ChronosTime] r =>
  Members [Executions, Log, Race] r =>
  InterpreterFor (PScoped TmuxMonitorTask TmuxMonitorTask TmuxMonitor !! TmuxMonitorError Void) r
interpretTmuxMonitorNoLog =
  interpretPScopedResumable_ pure \ TmuxMonitorTask {..} -> \case
    Wait -> do
      startLog ident pane
      waitBasic ident shellPid
