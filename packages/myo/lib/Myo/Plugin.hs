module Myo.Plugin where

import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Conc (interpretAtomic, interpretSyncAs, withAsync_)
import Data.MessagePack (Object)
import Ribosome (
  Errors,
  Execution (Async),
  Handler,
  HostError,
  MappingIdent,
  Persist,
  PersistError,
  RemoteStack,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  WatchedVariable,
  interpretNvimPlugin,
  interpretPersist,
  interpretPersistPath,
  reportError,
  rpc,
  runNvimPluginIO,
  )
import Ribosome.Data.PersistPathError (PersistPathError)
import Ribosome.Persist (PersistPath)
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.StoreHistoryLock (StoreHistoryLock (StoreHistoryLock))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Command.Output (myoOutputQuit, myoOutputSelect)
import Myo.Command.Update (updateCommands)
import Myo.Data.Env (Env)
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
import qualified Myo.Settings as Settings
import Myo.Temp (interpretLogDir)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (detectDefaultUi)
import Myo.Ui.Update (updateUi)

-- rpcHandlers :: [[RpcDef (Ribo Env Error)]]
-- rpcHandlers =
--   [
--     $(rpcHandler (cmd []) 'myoLogs),
--     $(rpcHandlerDef 'myoTogglePane),
--     $(rpcHandlerDef 'myoToggleLayout),
--     $(rpcHandler (cmd []) 'myoFocus),
--     $(rpcHandlerDef 'myoReRun),
--     $(rpcHandlerDef 'myoLine),
--     $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myoLineCmd),
--     $(rpcHandler (cmd [CmdComplete "shellcmd"]) 'myo),
--     $(rpcHandlerDef 'myoParse),
--     $(rpcHandler (cmd []) 'myoParseLatest),
--     $(rpcHandlerDef 'myoPrev),
--     $(rpcHandlerDef 'myoNext),
--     $(rpcHandlerDef 'myoSave),
--     $(rpcHandlerDef 'myoVimTest),
--     $(rpcHandler (cmd []) 'myoHistory),
--     $(rpcHandler (cmd []) 'myoCommands),
--     $(rpcHandlerDef 'myoStage4),
--     $(rpcHandler sync 'myoTestDetermineRunner),
--     $(rpcHandler sync 'myoTestExecutable),
--     $(rpcHandler sync 'myoTestBuildPosition),
--     $(rpcHandler sync 'myoTestBuildArgs),
--     $(rpcHandler (autocmd "VimLeavePre" . sync) 'myoQuit),
    -- rpcAutocmd "MyoQuit" Sync "VimLeavePre" myoQuit
--     $(rpcHandler (autocmd "BufWritePre") 'myoSave),
--     $(rpcHandler (
--     autocmd "User" . autocmdOptions (AutocmdOptions "MyoProject" False Nothing)) 'myoMyoLoaded)
--     ]

type MyoStack =
  [
    Executions,
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    Reader LogDir,
    NativeCodecE (Panes PaneMode),
    NativeCodecE (Panes PanePid),
    Proc !! ProcError,
    Backend !! RunError,
    Sync SaveLock,
    Sync StoreHistoryLock
  ]

handlers ::
  Members MyoStack r =>
  Members [Settings !! se, Scratch !! RpcError, Errors] r =>
  [RpcHandler r]
handlers =
  rpc "ProDiag" Async myoDiag
  <>
  rpc "MyoAddSystemCommand" Async myoAddSystemCommand
  <>
  rpc "MyoAddShellCommand" Async myoAddShellCommand
  -- <>
  -- completeWith CompleteFiltered myoCompleteCommand (rpc "MyoRun" Async myoRun)

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

prepare ::
  Members [Settings !! SettingError, AtomicState UiState, DataLog HostError] r =>
  Sem r ()
prepare = do
  resuming @_ @Settings (reportError (Just "ui")) do
    detect <- Settings.get Settings.detectUi
    when detect detectDefaultUi
    -- loadHistory

type MyoProdStack =
  [
    Parsing !! OutputError,
    Controller !! RunError,
    CommandLog,
    Persist [HistoryEntry] !! PersistError,
    PersistPath !! PersistPathError
  ] ++ MyoStack

interpretMyoStack ::
  InterpretersFor MyoStack RemoteStack
interpretMyoStack =
  interpretSyncAs StoreHistoryLock .
  interpretSyncAs SaveLock .
  interpretBackendFail .
  interpretProc .
  interpretCodecPanes .
  interpretCodecPanes .
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

interpretMyoProd ::
  InterpretersFor MyoProdStack RemoteStack
interpretMyoProd =
  interpretMyoStack .
  interpretPersistPath True .
  interpretPersist "history" .
  interpretCommandLogSetting .
  interpretController .
  interpretParsing parsers .
  withAsync_ prepare

myo :: IO ()
myo =
  runNvimPluginIO @MyoProdStack "myo" (interpretMyoProd . interpretNvimPlugin handlers mappings variables)
