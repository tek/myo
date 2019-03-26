{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Error where

import Data.DeepPrisms (deepPrisms)
import Ribosome.Error.Report.Class (ReportError(..))

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.OutputError (OutputError)
import Myo.Command.Data.RunError (RunError)

data Error =
  Run RunError
  |
  Command CommandError
  |
  Output OutputError
  deriving Show

deepPrisms ''Error

instance ReportError Error where
  errorReport (Run e) = errorReport e
  errorReport (Command e) = errorReport e
  errorReport (Output e) = errorReport e
