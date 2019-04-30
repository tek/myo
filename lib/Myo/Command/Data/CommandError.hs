{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.CommandError where

import Chiasma.Data.Ident (Ident, identText)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import System.Log (Priority(ERROR, NOTICE))

data CommandError =
  Misc Text
  |
  NoSuchCommand Ident
  |
  NoCommands
  |
  NoSuchHistoryIndex Int
  deriving (Eq, Show)

deepPrisms ''CommandError

instance ReportError CommandError where
  errorReport (Misc err) =
    ErrorReport (pre <> " " <> err) [pre, err] ERROR
    where
      pre = "command error:"
  errorReport (NoSuchCommand ident) =
    ErrorReport err [err] NOTICE
    where
      err = "no command with ident `" <> (toText . identText) ident <> "`"
  errorReport NoCommands =
    ErrorReport err [err] NOTICE
    where
      err = "no commands have been created yet"
  errorReport (NoSuchHistoryIndex index) =
    ErrorReport err [err] NOTICE
    where
      err = "no history entry at index " <> show index
