module Myo.Command.Data.CommandState where

import Chiasma.Data.Ident (Ident)
import Control.Concurrent.STM.TMChan (TMChan)
import Path (Abs, File, Path)
import Ribosome.Data.Syntax (Syntax)
import qualified Text.Show (show)

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
    command :: Ident,
    syntax :: [Syntax],
    events :: OutputEvents,
    currentEvent :: EventIndex.Absolute,
    report :: Maybe ParseReport
  }
  deriving stock (Eq, Show)

data CommandState =
  CommandState {
    commands :: [Command],
    history :: [HistoryEntry],
    logPaths :: LogPaths,
    logs :: Logs,
    output :: Maybe OutputState,
    outputHandlers :: Map CommandLanguage [OutputHandler],
    monitorChan :: Maybe (TMChan MonitorEvent),
    executing :: Map Ident Execution,
    executionLog :: [Execution]
  }
  deriving stock (Generic)
  deriving anyclass (Default)

instance Show CommandState where
  show (CommandState cmds hist lps lgs out han _ _ _) =
    "CommandState" ++ show (cmds, hist, lps, lgs, out, han)
