module Myo.Command.Data.CommandError(
  CommandError(..),
) where

import System.Log (Priority(ERROR, NOTICE))
import Chiasma.Data.Ident (Ident, identString)
import Ribosome.Error.Report (ReportError(..), ErrorReport(ErrorReport))

data CommandError =
  CommandError String
  |
  NoSuchCommand Ident
  deriving (Eq, Show)

instance ReportError CommandError where
  errorReport (CommandError err) =
    ErrorReport (pre ++ " " ++ err) [pre, err] ERROR
    where
      pre = "command error:"
  errorReport (NoSuchCommand ident) =
    ErrorReport err [err] NOTICE
    where
      err = "no command with ident `" ++ identString ident ++ "`"
