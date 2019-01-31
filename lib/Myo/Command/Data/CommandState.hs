{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.CommandState(
  CommandState(..),
  Logs,
  _commands,
  _history,
  _logs,
  _running,
) where

import Chiasma.Data.Ident (Ident)
import Control.Lens (makeClassy_)
import Data.Default (Default)
import Data.Map (Map)
import GHC.Generics (Generic)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.RunningCommand (RunningCommand)

type Logs = Map Ident FilePath

data CommandState =
  CommandState {
    commands :: [Command],
    history :: [HistoryEntry],
    logs :: Logs,
    running :: [RunningCommand]
  }
  deriving (Eq, Show, Generic, Default)

makeClassy_ ''CommandState
