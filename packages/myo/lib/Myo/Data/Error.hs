module Myo.Data.Error where

import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Msgpack.Error (DecodeError)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.RunError (RunError)
import Myo.Output.Data.OutputError (OutputError)

data Error =
  Run RunError
  |
  Command CommandError
  |
  Output OutputError
  |
  Mapping MappingError
  |
  Decode DecodeError
  |
  Rpc RpcError
  |
  Persist PersistError
  deriving (Show, Generic, ReportError)

deepPrisms ''Error
