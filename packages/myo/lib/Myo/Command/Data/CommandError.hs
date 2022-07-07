module Myo.Command.Data.CommandError where

import Chiasma.Data.Ident (Ident, identText)
import Exon (exon)
import Log (Severity (Error, Warn, Info))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

import Myo.Command.Data.Command (shortIdent)

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
  |
  NotAUiShell Ident Ident
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
    ErrorMessage err ["CommandError.NoHistory"] Info
    where
      err = "no history yet"
  toErrorMessage (NotAUiShell cmd shell) =
    ErrorMessage msg log Error
    where
      msg =
        [exon|`#{shortIdent shell}` cannot be used as a shell for `#{shortIdent cmd}`|]
      log =
        ["CommandError.NotAUiShell:", [exon|cmd: #{identText cmd}|], [exon|shell: #{identText shell}|]]
