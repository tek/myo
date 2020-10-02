{-# LANGUAGE DeriveAnyClass #-}

module Myo.Command.Data.CommandState where

import Control.Concurrent.STM.TMChan (TMChan)
import Path (Abs, File, Path)
import Ribosome.Data.Syntax (Syntax)
import qualified Text.Show (Show(show))

import Myo.Command.Data.Command (Command, CommandLanguage)
import Myo.Command.Data.CommandLog (CommandLog)
import Myo.Command.Data.Execution (Execution)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.MonitorEvent (MonitorEvent)
import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute)
import Myo.Output.Data.OutputEvents (OutputEvents)
import Myo.Output.Data.OutputHandler (OutputHandler)
import Myo.Output.Data.ParseReport (ParseReport)

type LogPaths = Map Ident (Path Abs File)
type Logs = Map Ident CommandLog

data OutputState =
  OutputState {
    _command :: Ident,
    _syntax :: [Syntax],
    _events :: OutputEvents,
    _currentEvent :: EventIndex.Absolute,
    _report :: Maybe ParseReport
  }
  deriving (Eq, Show)

deepLenses ''OutputState

data CommandState =
  CommandState {
    _commands :: [Command],
    _history :: [HistoryEntry],
    _logPaths :: LogPaths,
    _logs :: Logs,
    _output :: Maybe OutputState,
    _outputHandlers :: Map CommandLanguage [OutputHandler],
    _monitorChan :: Maybe (TMChan MonitorEvent),
    _executing :: Map Ident Execution,
    _executionLog :: [Execution]
  }
  deriving (Generic, Default)

deepLenses ''CommandState

instance Text.Show.Show CommandState where
  show (CommandState cmds hist lps lgs out han _ _ _) =
    "CommandState" ++ show (cmds, hist, lps, lgs, out, han)
