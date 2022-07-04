module Myo.Plugin where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxNative (TmuxNative (TmuxNative))
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Interpreter.Codec (interpretCodecPanes, interpretCodecTmuxCommand)
import Chiasma.Interpreter.TmuxClient (interpretTmuxNative)
import Conc (Restoration, interpretAtomic, interpretSyncAs, withAsync_)
import Data.MessagePack (Object)
import Path (relfile)
import Polysemy.Chronos (ChronosTime)
import Process (resolveExecutable)
import Ribosome (
  BootError (BootError),
  CompleteStyle (CompleteFiltered),
  Errors,
  Execution (Async, Sync),
  Handler,
  HostError,
  MappingIdent,
  Persist,
  PersistError,
  PersistPath,
  PersistPathError,
  RemoteStack,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  WatchedVariable,
  completeBuiltin,
  completeWith,
  interpretNvimPlugin,
  interpretPersist,
  interpretPersistPath,
  reportError,
  rpc,
  rpcCommand,
  rpcFunction,
  runNvimPluginIO, rpcAutocmd,
  )
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.CommandMenu (myoCommands)
import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.StoreHistoryLock (StoreHistoryLock (StoreHistoryLock))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.HistoryMenu (myoHistory)
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Command.Log (myoLogs)
import Myo.Command.Output (myoNext, myoOutputQuit, myoOutputSelect, myoPrev)
import Myo.Command.Parse (myoParse, myoParseLatest)
import Myo.Command.Run (myoLine, myoLineCmd, myoReRun, myoRun)
import Myo.Command.Test (myoVimTest, myoTestDetermineRunner, myoTestExecutable, myoTestBuildPosition, myoTestBuildArgs)
import Myo.Command.Update (updateCommands)
import Myo.Complete (myoCompleteCommand)
import Myo.Data.Env (Env)
import Myo.Data.LastSave (LastSave)
import Myo.Data.ProcError (ProcError)
import Myo.Data.SaveLock (SaveLock (SaveLock))
import Myo.Diag (myoDiag)
import Myo.Effect.Controller (Controller)
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.Controller (interpretController)
import Myo.Interpreter.Proc (interpretProc)
import Myo.Output.Data.OutputError (OutputError)
import Myo.Output.Effect.Parsing (OutputParser, Parsing)
import Myo.Output.Interpreter.Parsing (interpretParsing)
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.Lang.Scala.Parser (scalaOutputParser)
import Myo.Save (myoSave)
import qualified Myo.Settings as Settings
import Myo.Temp (interpretLogDir)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (detectDefaultUi)
import Myo.Ui.Focus (myoFocus)
import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)
import Myo.Ui.Update (updateUi)
import Myo.Quit (myoQuit)

type MyoStack =
  [
    Executions,
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
    NativeCodecE TmuxCommand,
    NativeCodecE (Panes PaneMode),
    NativeCodecE (Panes PanePid),
    NativeCodecE (Panes Pane),
    Proc !! ProcError,
    Backend !! RunError,
    Sync SaveLock,
    Sync StoreHistoryLock
  ]

type MyoProdStack =
  [
    Parsing !! OutputError,
    Controller !! RunError,
    CommandLog,
    Persist [HistoryEntry] !! PersistError,
    PersistPath !! PersistPathError,
    NativeTmux !! TmuxError,
    Reader TmuxNative,
    AtomicState LastSave
  ] ++ MyoStack

handlers ::
  Members MyoProdStack r =>
  Members [Settings !! SettingError, Scratch !! RpcError, Rpc !! RpcError, DataLog HostError, Errors] r =>
  Members [ChronosTime, Mask Restoration, Log, Resource, Race, Embed IO, Final IO] r =>
  [RpcHandler r]
handlers =
  rpc "MyoDiag" Async myoDiag
  <>
  completeWith CompleteFiltered myoCompleteCommand (rpcCommand "MyoRun" Async myoRun)
  <>
  rpc "MyoReRun" Async myoReRun
  <>
  rpc "MyoLine" Async myoLine
  <>
  rpc "MyoVimTest" Async myoVimTest
  <>
  rpc "MyoAddSystemCommand" Async myoAddSystemCommand
  <>
  rpc "MyoAddShellCommand" Async myoAddShellCommand
  <>
  rpc "MyoLogs" Async myoLogs
  <>
  rpc "MyoParse" Async myoParse
  <>
  rpc "MyoParseLatest" Async myoParseLatest
  <>
  rpc "MyoPrev" Async myoPrev
  <>
  rpc "MyoNext" Async myoNext
  <>
  rpc "MyoHistory" Sync myoHistory
  <>
  rpc "MyoCommands" Sync myoCommands
  <>
  rpc "MyoTogglePane" Async myoTogglePane
  <>
  rpc "MyoToggleLayout" Async myoToggleLayout
  <>
  rpc "MyoFocus" Async myoFocus
  <>
  rpc "MyoSave" Async myoSave
  <>
  [
    rpcFunction "MyoRun" Async myoRun,
    completeBuiltin "shellcmd" (rpcCommand "Myo" Async myoLineCmd),
    rpcFunction "MyoTestDetermineRunner" Sync myoTestDetermineRunner,
    rpcFunction "MyoTestExecutable" Sync myoTestExecutable,
    rpcFunction "MyoTestBuildPosition" Sync myoTestBuildPosition,
    rpcFunction "MyoTestArgs" Sync myoTestBuildArgs,
    rpcAutocmd "MyoQuit" Sync "VimLeavePre" def myoQuit,
    rpcAutocmd "MyoSave" Async "BufWritePre" def myoSave
  ]

mappings ::
  Members [AtomicState CommandState, Scratch !! RpcError, Rpc !! RpcError, Embed IO] r =>
  Map MappingIdent (Handler r ())
mappings =
  [
    ("output-quit", myoOutputQuit),
    ("output-select", myoOutputSelect)
  ]

variables ::
  Members [AtomicState CommandState, AtomicState UiState, Settings !! SettingError, Embed IO] r =>
  Map WatchedVariable (Object -> Handler r ())
variables =
  [
    ("myo_commands", updateCommands),
    ("myo_ui", updateUi)
  ]

-- TODO load history
prepare ::
  Members [Settings !! SettingError, AtomicState UiState, DataLog HostError] r =>
  Sem r ()
prepare = do
  resuming @_ @Settings (reportError (Just "ui")) do
    detect <- Settings.get Settings.detectUi
    when detect detectDefaultUi
    -- loadHistory

interpretMyoStack ::
  InterpretersFor MyoStack RemoteStack
interpretMyoStack =
  interpretSyncAs StoreHistoryLock .
  interpretSyncAs SaveLock .
  interpretBackendFail .
  interpretProc .
  interpretCodecPanes .
  interpretCodecPanes .
  interpretCodecPanes .
  interpretCodecTmuxCommand .
  interpretLogDir .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretExecutions

parsers ::
  Member (Embed IO) r =>
  Map CommandLanguage [OutputParser r]
parsers =
  [
    ("haskell", [haskellOutputParser]),
    ("nix", [nixOutputParser]),
    ("scala", [scalaOutputParser])
  ]

-- TODO Add tmux interpreter that always stops if the executable can't be found, so that the plugin may start without
-- support for tmux
withTmuxSocket ::
  Members [Error BootError, Embed IO] r =>
  InterpreterFor (Reader TmuxNative) r
withTmuxSocket sem = do
  exe <- fromEither . first BootError =<< resolveExecutable [relfile|tmux|] Nothing
  runReader (TmuxNative exe Nothing) sem

interpretMyoProd ::
  InterpretersFor MyoProdStack RemoteStack
interpretMyoProd =
  interpretMyoStack .
  interpretAtomic def .
  withTmuxSocket .
  interpretTmuxNative .
  interpretPersistPath True .
  interpretPersist "history" .
  interpretCommandLogSetting .
  interpretController .
  interpretParsing parsers .
  withAsync_ prepare

myo :: IO ()
myo =
  runNvimPluginIO @MyoProdStack "myo" (interpretMyoProd . interpretNvimPlugin handlers mappings variables)
