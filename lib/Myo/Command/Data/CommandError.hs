
module Myo.Command.Data.CommandError where

import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(ERROR, NOTICE))

data CommandError =
  Misc Text
  |
  NoSuchCommand Text Text
  |
  NoCommands
  |
  NoSuchHistoryIndex Int
  |
  NoSuchHistoryIdent Text
  |
  NoHistory
  deriving (Eq, Show)

deepPrisms ''CommandError

instance ReportError CommandError where
  errorReport (Misc err) =
    ErrorReport (pre <> " " <> err) [pre, err] ERROR
    where
      pre = "command error:"
  errorReport (NoSuchCommand context ident) =
    ErrorReport err ["In context `" <> context <>"`:", err] NOTICE
    where
      err = "no command with ident `" <> ident <> "`"
  errorReport NoCommands =
    ErrorReport err [err] NOTICE
    where
      err = "no commands have been created yet"
  errorReport (NoSuchHistoryIndex index) =
    ErrorReport err [err] NOTICE
    where
      err = "no history entry at index " <> show index
  errorReport (NoSuchHistoryIdent ident) =
    ErrorReport err [err] NOTICE
    where
      err = "no history entry with ident `" <> ident <> "`"
  errorReport NoHistory =
    ErrorReport err ["CommandError.NoHistory"] NOTICE
    where
      err = "no history yet"
