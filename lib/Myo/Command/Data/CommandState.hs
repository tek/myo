{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.CommandState(
  CommandState(..),
  Logs,
  Parser,
  commands,
  history,
  logs,
  parseResult,
  parsers,
  running,
) where

import Chiasma.Data.Ident (Ident)
import Data.DeepLenses (deepLenses)
import Data.Default (Default)
import Data.Map (Map)
import GHC.Generics (Generic)

import Myo.Command.Data.Command (Command, CommandLanguage)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.ParsedOutput (ParsedOutput)
import Myo.Command.Data.RunningCommand (RunningCommand)

type Logs = Map Ident FilePath
type Parser = [String] -> ParsedOutput

data CommandState =
  CommandState {
    _commands :: [Command],
    _history :: [HistoryEntry],
    _logs :: Logs,
    _running :: [RunningCommand],
    _parseResult :: Maybe ParsedOutput,
    _parsers :: Map CommandLanguage [Parser]
  }
  deriving (Generic, Default)

deepLenses ''CommandState
