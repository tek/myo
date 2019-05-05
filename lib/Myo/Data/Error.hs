{-# LANGUAGE TemplateHaskell #-}

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
  deriving Show

deepPrisms ''Error

instance ReportError Error where
  errorReport (Run e) = errorReport e
  errorReport (Command e) = errorReport e
  errorReport (Output e) = errorReport e
  errorReport (Mapping e) = errorReport e
  errorReport (Decode e) = errorReport e
  errorReport (Rpc e) = errorReport e
  errorReport (Persist e) = errorReport e
