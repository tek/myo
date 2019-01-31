module Myo.Command.Data.RunError(
  RunError(..),
) where

import System.Log (Priority(ERROR))
import Ribosome.Error.Report (ReportError(..), ErrorReport(ErrorReport))

import Myo.Command.Data.CommandError (CommandError(..))
import Myo.Command.Data.RunTask (RunTask)

data RunError =
  RunError String
  |
  Command CommandError
  |
  NoRunner RunTask
  deriving (Eq, Show)

instance ReportError RunError where
  errorReport (RunError e) =
    ErrorReport e ["running command failed:", e] ERROR
  errorReport (Command e) = errorReport e
