{-# LANGUAGE DeriveAnyClass #-}

module Myo.Data.Error where

import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.Mapping (MappingError)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

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
