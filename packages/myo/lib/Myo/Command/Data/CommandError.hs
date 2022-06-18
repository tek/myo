module Myo.Command.Data.CommandError where

import Log (Severity (Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

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
  deriving stock (Eq, Show)

instance ToErrorMessage CommandError where
  toErrorMessage (Misc err) =
    ErrorMessage (pre <> " " <> err) [pre, err] Error
    where
      pre = "command error:"
  toErrorMessage (NoSuchCommand context ident) =
    ErrorMessage err ["In context `" <> context <>"`:", err] Warn
    where
      err = "no command with ident `" <> ident <> "`"
  toErrorMessage NoCommands =
    ErrorMessage err [err] Warn
    where
      err = "no commands have been created yet"
  toErrorMessage (NoSuchHistoryIndex index) =
    ErrorMessage err [err] Warn
    where
      err = "no history entry at index " <> show index
  toErrorMessage (NoSuchHistoryIdent ident) =
    ErrorMessage err [err] Warn
    where
      err = "no history entry with ident `" <> ident <> "`"
  toErrorMessage NoHistory =
    ErrorMessage err ["CommandError.NoHistory"] Warn
    where
      err = "no history yet"
