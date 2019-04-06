{-# LANGUAGE TemplateHaskell #-}

module Myo.Output.Data.OutputError where

import Chiasma.Data.Ident (Ident, identString)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(NOTICE))

import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)

data OutputError =
  Command CommandError
  |
  NoLang Ident
  |
  Setting SettingError
  |
  NoHandler CommandLanguage
  |
  Parse String
  |
  NoEvents Ident
  |
  NoOutput Ident
  deriving Show

deepPrisms ''OutputError

instance ReportError OutputError where
  errorReport (Command e) =
    errorReport e
  errorReport (NoLang ident) =
    ErrorReport msg [msg] NOTICE
    where
      msg = "command `" ++ identString ident ++ "` has no language"
  errorReport (Setting e) =
    errorReport e
  errorReport (NoHandler (CommandLanguage lang)) =
    ErrorReport msg [msg] NOTICE
    where
      msg = "no output handler for language `" ++ lang ++ "`"
  errorReport (Parse err) =
    ErrorReport msg ["OutputError.Parse:", err] NOTICE
    where
      msg = "failed to parse command output"
  errorReport (NoEvents ident) =
    ErrorReport msg [msg] NOTICE
    where
      msg = "no events in output of command `" ++ identString ident ++ "`"
  errorReport (NoOutput ident) =
    ErrorReport msg [msg] NOTICE
    where
      msg = "command `" ++ identString ident ++ "` has not generated any output"
