{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.CommandState where

import Chiasma.Data.Ident (Ident)
import Control.Concurrent.STM.TMChan (TMChan)
import Data.DeepLenses (deepLenses)
import Data.Default (Default)
import Data.Map (Map)
import GHC.Generics (Generic)
import Path (Abs, File, Path)
import qualified Text.Show (Show(show))

import Myo.Command.Data.Command (Command, CommandLanguage)
import Myo.Command.Data.CommandLog (CommandLog)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.RunningCommand (RunningCommand)
import Myo.Output.Data.OutputEvent (EventIndex)
import Myo.Output.Data.OutputHandler (OutputHandler)
import Myo.Output.Data.ParseReport (ParseReport)
import Myo.Output.Data.ParseResult (ParseResult)
import Myo.Ui.Data.PaneOutput (PaneOutput)

type LogPaths = Map Ident (Path Abs File)
type Logs = Map Ident CommandLog

data CommandState =
  CommandState {
    _commands :: [Command],
    _history :: [HistoryEntry],
    _logPaths :: LogPaths,
    _logs :: Logs,
    _running :: [RunningCommand],
    _parseResult :: Maybe ParseResult,
    _parseReport :: Maybe ParseReport,
    _currentEvent :: EventIndex,
    _outputHandlers :: Map CommandLanguage [OutputHandler],
    _watcherChan :: Maybe (TMChan PaneOutput)
  }
  deriving (Generic, Default)

deepLenses ''CommandState

instance Text.Show.Show CommandState where
  show (CommandState cmds hist lps ls rn _ prprt ce han _) =
    "CommandState" ++ show (cmds, hist, lps, ls, rn, prprt, ce, han)
