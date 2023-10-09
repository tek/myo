module Myo.Command.Data.CommandState where

import Prelude hiding (output)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.OutputState (OutputState)
import Myo.Data.CommandId (CommandId)

data CommandState =
  CommandState {
    commands :: Map CommandId Command,
    output :: Maybe OutputState
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)
