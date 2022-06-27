module Myo.Command.Interpreter.TmuxMonitor where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.PipePaneParams (target)
import qualified Chiasma.Data.Target as Target
import Chiasma.Data.TmuxCommand (TmuxCommand (PipePane))
import Chiasma.Data.TmuxId (PaneId, formatId)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import qualified Chiasma.Effect.TmuxApi as Tmux
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Conc (PScoped, interpretPScopedResumableWith, race_)
import Control.Lens.Regex.ByteString (match, regex)
import Exon (exon)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Process (Pid)
import Time (MilliSeconds (MilliSeconds), while)

import qualified Myo.Command.Data.ExecutionState as ExecutionState
import Myo.Command.Data.TmuxMonitorError (TmuxMonitorError (SocketReader, TmuxCodec))
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Effect.Executions (Executions)
import qualified Myo.Command.Effect.SocketReader as SocketReader
import Myo.Command.Effect.SocketReader (ScopedSocketReader, SocketReader, socketReader)
import Myo.Command.Effect.TmuxMonitor (TmuxMonitor (Wait), TmuxMonitorTask (TmuxMonitorTask), ident, pane, shellPid)
import Myo.Command.Log (pipePaneToSocket)
import Myo.Data.ProcError (ProcError)
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
  [regex|\r\n|] . match .~ "\n"

readOutput ::
  Members [SocketReader, CommandLog] r =>
  Ident ->
  Sem r ()
readOutput ident =
  useWhileJust SocketReader.chunk (CommandLog.append ident . sanitizeOutput)

pipingToSocket ::
  Members [NativeTmux, SocketReader, NativeCommandCodecE, Stop CodecError, Resource] r =>
  PaneId ->
  Sem r a ->
  Sem r a
pipingToSocket pane =
  bracket_ acquire release
  where
    acquire = do
      socket <- SocketReader.path
      withTmux_ do
        pipePaneToSocket pane socket
    release =
      withTmux_ do
        Tmux.send (PipePane def { target = Target.Pane pane })

monitorResources ::
  ∀ socket sre r a .
  Members [NativeTmux, ScopedSocketReader socket !! sre, NativeCommandCodecE, Resource] r =>
  TmuxMonitorTask ->
  (TmuxMonitorTask -> Sem (SocketReader : Stop (TmuxMonitorError sre) : r) a) ->
  Sem (Stop (TmuxMonitorError sre) : r) a
monitorResources task@TmuxMonitorTask {..} use =
  resumeHoist SocketReader $ mapStop TmuxCodec do
    socketReader ident $ pipingToSocket pane do
      insertAt @1 (use task)

interpretTmuxMonitor ::
  Members [Proc !! ProcError, ChronosTime, CommandLog] r =>
  Members [Executions, NativeTmux, ScopedSocketReader socket !! sre, NativeCommandCodecE, Resource, Log, Race] r =>
  InterpreterFor (PScoped TmuxMonitorTask TmuxMonitorTask TmuxMonitor !! TmuxMonitorError sre) r
interpretTmuxMonitor =
  interpretPScopedResumableWith @ScopeEffects monitorResources \ TmuxMonitorTask {..} -> \case
    Wait -> do
      Log.debug [exon|Monitoring tmux command `#{identText ident}` in #{formatId pane}|]
      race_ (Executions.waitKill ident) (race_ (pollPid ident shellPid) (readOutput ident))
