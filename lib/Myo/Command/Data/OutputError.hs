{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.OutputError(
  OutputError(..),
) where

import Chiasma.Data.Ident (Ident, identString)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import System.Log (Priority(NOTICE))

import Myo.Command.Data.CommandError (CommandError)

data OutputError =
  Command CommandError
  |
  NoLang Ident
  |
  Setting SettingError
  |
  Rpc RpcError
  deriving Show

deepPrisms ''OutputError

instance ReportError OutputError where
  errorReport (Command e) =
    errorReport e
  errorReport (NoLang ident) =
    ErrorReport msg [msg] NOTICE
    where
      msg = "command `" ++ identString ident ++ "` has no language"
  errorReport (Rpc e) =
    errorReport e
  errorReport (Setting e) =
    errorReport e
