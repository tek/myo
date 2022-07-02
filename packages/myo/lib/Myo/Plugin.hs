{-# OPTIONS_GHC -Wno-unused-imports #-}
module Myo.Plugin where

import Chiasma.Codec.Data.PaneMode (PaneMode)
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (Codec, NativeCodec, NativeCodecE)
import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Conc (ChanConsumer, ChanEvents, interpretAtomic, interpretEventsChan, interpretSyncAs, withAsync_)
import Polysemy.Chronos (ChronosTime)
import Ribosome (
  BootError,
  CompleteStyle (CompleteFiltered),
  Errors,
  Execution (Async),
  HostError,
  Rpc,
  RpcError,
  RpcHandler,
  Scratch,
  SettingError,
  Settings,
  reportError,
  rpc,
  runNvimHandlersIO,
  )
import qualified Ribosome.Settings as Settings

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunEvent (RunEvent)
import Myo.Command.Data.StoreHistoryLock (StoreHistoryLock (StoreHistoryLock))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Effect.Executions (Executions)
import Myo.Command.Interpreter.Backend.Generic (interpretBackendFail)
import Myo.Command.Interpreter.Executions (interpretExecutions)
-- import Myo.Command.Run (myoRun)
import Myo.Data.Env (Env)
import Myo.Data.ProcError (ProcError)
import Myo.Data.SaveLock (SaveLock (SaveLock))
import Myo.Diag (myoDiag)
import Myo.Effect.Proc (Proc)
import Myo.Interpreter.Proc (interpretProc)
import qualified Myo.Settings as Settings
import Myo.Temp (interpretLogDir)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (detectDefaultUi)

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

-- mappingOutputQuit ::
--   MappingHandler m
-- mappingOutputQuit =
--   mappingHandler "output-quit" outputQuit

-- mappingOutputSelect ::
--   MappingHandler m
-- mappingOutputSelect =
--   mappingHandler "output-select" outputSelect

-- mappings ::
--   [MappingHandler m]
-- mappings =
--   [mappingOutputQuit, mappingOutputSelect]

-- variables ::
--   Map Text (Object -> m ())
-- variables =
--   Map.fromList [("myo_commands", updateCommands), ("myo_ui", updateUi)]

type MyoStack =
  [
    Executions,
    AtomicState Env,
    AtomicState UiState,
    AtomicState Views,
    AtomicState CommandState,
    ChanEvents RunEvent,
    ChanConsumer RunEvent,
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


prepare ::
  Members [Settings !! SettingError, AtomicState UiState, DataLog HostError] r =>
  Sem r ()
prepare = do
  resuming @_ @Settings (reportError (Just "ui")) do
    detect <- Settings.get Settings.detectUi
    when detect detectDefaultUi

interpretMyoStack ::
  Members [DataLog HostError, ChronosTime] r =>
  Members [Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor MyoStack r
interpretMyoStack sem =
  interpretSyncAs StoreHistoryLock $
  interpretSyncAs SaveLock $
  interpretBackendFail $
  interpretProc $
  interpretCodecPanes $
  interpretCodecPanes $
  interpretLogDir $
  interpretEventsChan $
  interpretAtomic def $
  interpretAtomic def $
  interpretAtomic def $
  interpretAtomic def $
  interpretExecutions do
    withAsync_ prepare sem

myo :: IO ()
myo =
  runNvimHandlersIO @MyoStack "myo" interpretMyoStack handlers
