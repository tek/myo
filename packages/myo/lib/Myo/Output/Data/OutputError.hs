module Myo.Output.Data.OutputError where

import Chiasma.Data.Ident (Ident, identText)
import Log (Severity (Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), SettingError, ToErrorMessage (..))

import Myo.Command.Data.Command (CommandLanguage (CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.RunError (RunError)

data OutputError =
  Command CommandError
  |
  Run RunError
  |
  NoLang Ident
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

instance ToErrorMessage OutputError where
  toErrorMessage (Command e) =
    toErrorMessage e
  toErrorMessage (Run e) =
    toErrorMessage e
  toErrorMessage (NoLang ident) =
    ErrorMessage msg [msg] Warn
    where
      msg = "command `" <> identText ident <> "` has no language"
  toErrorMessage (Setting e) =
    toErrorMessage e
  toErrorMessage (NoHandler (CommandLanguage lang)) =
    ErrorMessage msg [msg] Warn
    where
      msg = "no output handler for language `" <> lang <> "`"
  toErrorMessage (Parse err) =
    ErrorMessage msg ["OutputError.Parse:", err] Warn
    where
      msg = "failed to parse command output"
  toErrorMessage (NoEvents ident) =
    ErrorMessage msg [msg] Warn
    where
      msg = "no events in output of command `" <> ident <> "`"
  toErrorMessage (NoOutput ident) =
    ErrorMessage msg [msg] Warn
    where
      msg = "command `" <> ident <> "` has not generated any output"
  toErrorMessage NoLocation =
    ErrorMessage "this event is not associated with a location" ["OutputError.NoLocation"] Warn
  toErrorMessage FileNonexistent =
    ErrorMessage "this event's file is nonexistent" ["OutputError.FileNonexistent"] Warn
  toErrorMessage (Internal msg) =
    ErrorMessage "internal error" ["OutputError.Internal:", msg] Error
  toErrorMessage NotParsed =
    ErrorMessage "no command output has been parsed" ["OutputError.NotParsed"] Warn
