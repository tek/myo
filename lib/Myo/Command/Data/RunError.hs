
module Myo.Command.Data.RunError where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Orphans ()
import System.Log (Priority(NOTICE, DEBUG, ERROR))

import qualified Myo.Command.Data.Command as Cmd (Command(Command))
import Myo.Command.Data.CommandError (CommandError(..))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Ui.Data.ToggleError (ToggleError)

data RunError =
  Command CommandError
  |
  NoRunner RunTask
  |
  Toggle ToggleError
  |
  Views ViewsError
  |
  Tmux TmuxError
  |
  IOEmbed Text
  |
  SocketFailure
  |
  InvalidShell Cmd.Command
  |
  InvalidCmdline Text
  |
  Unsupported Text Text
  |
  VimTest Text
  |
  NoLinesSpecified
  |
  SubprocFailed [Text]
  deriving Show

deepPrisms ''RunError

instance ReportError RunError where
  errorReport (Command e) = errorReport e
  errorReport (NoRunner task@(RunTask (Cmd.Command _ ident _ _ _ _ _ _ _) _ _)) =
    ErrorReport user ["no runner for task:", show task] NOTICE
    where
      user = "no runner available for command `" <> identText ident <> "`"
  errorReport (Toggle e) =
    errorReport e
  errorReport (Views e) = errorReport e
  errorReport (Tmux e) = errorReport e
  errorReport (IOEmbed e) =
    ErrorReport "internal error" ["embedded IO had unexpected error:", e] DEBUG
  errorReport SocketFailure =
    ErrorReport "internal error" ["could not create listener socket"] ERROR
  errorReport (InvalidShell command@(Cmd.Command _ ident _ _ _ _ _ _ _)) =
    ErrorReport msg ["RunError.InvalidShell:", show command] ERROR
    where
      msg = "invalid command for shell: " <> show ident
  errorReport (InvalidCmdline err) =
    ErrorReport msg [msg] NOTICE
    where
      msg =
        "invalid command line: " <> err
  errorReport (Unsupported runner tpe) =
    ErrorReport msg [msg] NOTICE
    where
      msg =
        "runner `" <> runner <> "` does not support " <> tpe <> " commands"
  errorReport (VimTest e) =
    ErrorReport "vim-test failed" ["RunError.VimTest:", e] ERROR
  errorReport NoLinesSpecified =
    ErrorReport "no lines specified for command" ["RunError.NoLinesSpecified"] NOTICE
  errorReport (SubprocFailed err) =
    ErrorReport ("subprocess failed" <> userErr err) ("RunError.SubprocFailed" : err) NOTICE
    where
      userErr [] = ""
      userErr (e : _) = ": " <> e
