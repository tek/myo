module Myo.Command.Data.ActiveTarget where

import Myo.Data.CommandId (CommandId)

data ActiveTarget =
  ActiveCommandShell CommandId (Maybe ActiveTarget)
  |
  ActiveCommand CommandId
  deriving stock (Eq, Show)
