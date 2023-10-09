module Myo.Output.Data.OutputError where

import Exon (exon)
import Log (Severity (Error, Warn))
import Ribosome (Report (Report), Reportable (..), SettingError)

import Myo.Command.Data.Command (CommandLanguage (CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.RunError (RunError)
import Myo.Data.CommandId (CommandId, commandIdText)

data OutputError =
  Command CommandError
  |
  Run RunError
  |
  NoLang CommandId
  |
  Setting SettingError
  |
  NoHandler CommandLanguage
  |
  Parse Text
  |
  NoEvents Text
  |
  NoOutput Text
  |
  NoLocation
  |
  FileNonexistent
  |
  Internal Text
  |
  NotParsed
  deriving stock (Show)

instance Reportable OutputError where
  toReport (Command e) =
    toReport e
  toReport (Run e) =
    toReport e
  toReport (NoLang ident) =
    Report msg [msg] Warn
    where
      msg = [exon|command '#{commandIdText ident}' has no language|]
  toReport (Setting e) =
    toReport e
  toReport (NoHandler (CommandLanguage lang)) =
    Report msg [msg] Warn
    where
      msg = "no output handler for language `" <> lang <> "`"
  toReport (Parse err) =
    Report msg ["OutputError.Parse:", err] Warn
    where
      msg = "failed to parse command output"
  toReport (NoEvents name) =
    Report msg [msg] Warn
    where
      msg = [exon|No events in output of command '#{name}'|]
  toReport (NoOutput ident) =
    Report msg [msg] Warn
    where
      msg = "command `" <> ident <> "` has not generated any output"
  toReport NoLocation =
    Report "this event is not associated with a location" ["OutputError.NoLocation"] Warn
  toReport FileNonexistent =
    Report "this event's file is nonexistent" ["OutputError.FileNonexistent"] Warn
  toReport (Internal msg) =
    Report "internal error" ["OutputError.Internal:", msg] Error
  toReport NotParsed =
    Report "no command output has been parsed" ["OutputError.NotParsed"] Warn
