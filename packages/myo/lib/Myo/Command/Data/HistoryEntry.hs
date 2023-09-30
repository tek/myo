module Myo.Command.Data.HistoryEntry where

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.Param (ParamValues)
import Myo.Data.CommandId (CommandId)

data ExecutionParams =
  ExecutionParams {
    id :: CommandId,
    compiled :: [Text],
    params :: ParamValues
  }
  deriving stock (Eq, Show, Generic)

json ''ExecutionParams

data HistoryEntry =
  HistoryEntry {
    command :: Command,
    -- | The lines and parameter values used when the command was last executed.
    execution :: Maybe ExecutionParams
  }
  deriving stock (Eq, Show, Generic)

json ''HistoryEntry
