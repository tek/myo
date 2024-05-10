module Myo.Command.Data.HistoryEntry where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))

import qualified Myo.Command.Data.Command
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
    execution :: ExecutionParams
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

simpleHistoryEntry :: Command -> HistoryEntry
simpleHistoryEntry command =
  HistoryEntry {command, execution = ExecutionParams {id = command.ident, compiled = [], params = mempty}}

instance FromJSON HistoryEntry where
  parseJSON = withObject "HistoryEntry" \ o -> do
    command <- o .: "command"
    o .:? "execution" <&> \case
      Just execution -> HistoryEntry {..}
      Nothing -> simpleHistoryEntry command
