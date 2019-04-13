{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.CommandState where

import Chiasma.Data.Ident (Ident)
import Control.Concurrent.STM.TMChan (TMChan)
import Data.DeepLenses (deepLenses)
import Data.Default (Default)
import Data.Map (Map)
import GHC.Generics (Generic)

import Myo.Command.Data.Command (Command, CommandLanguage)
import Myo.Command.Data.CommandLog (CommandLog)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.RunningCommand (RunningCommand)
import Myo.Output.Data.OutputHandler (OutputHandler)
import Myo.Output.Data.ParseReport (ParseReport)
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import Myo.Ui.Data.PaneOutput (PaneOutput)

type LogPaths = Map Ident FilePath
type Logs = Map Ident CommandLog

data CommandState =
  CommandState {
    _commands :: [Command],
    _history :: [HistoryEntry],
    _logPaths :: LogPaths,
    _logs :: Logs,
    _running :: [RunningCommand],
    _parsedOutput :: Maybe [ParsedOutput],
    _parseReports :: Maybe ParseReport,
    _outputHandlers :: Map CommandLanguage [OutputHandler],
    _watcherChan :: Maybe (TMChan PaneOutput)
  }
  deriving (Generic, Default)

deepLenses ''CommandState
