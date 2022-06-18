
module Myo.Command.Data.RunError where

import Chiasma.Data.Ident (identText)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Log (Severity (Debug, Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (..))

import qualified Myo.Command.Data.Command as Cmd (Command (Command))
import Myo.Command.Data.CommandError (CommandError (..))
import Myo.Command.Data.RunTask (RunTask (RunTask))
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
  deriving stock (Show)

instance ToErrorMessage RunError where
  toErrorMessage (Command e) = toErrorMessage e
  toErrorMessage (NoRunner task@(RunTask (Cmd.Command _ ident _ _ _ _ _ _ _) _ _)) =
    ErrorMessage user ["no runner for task:", show task] Warn
    where
      user = "no runner available for command `" <> identText ident <> "`"
  toErrorMessage (Toggle e) =
    toErrorMessage e
  toErrorMessage (Views e) =
      ErrorMessage "tmux error" ["RunError.Views:", show e] Error
  toErrorMessage (Tmux e) =
      ErrorMessage "tmux error" ["RunError.Tmux:", show e] Error
  toErrorMessage (IOEmbed e) =
    ErrorMessage "internal error" ["embedded IO had unexpected error:", e] Debug
  toErrorMessage SocketFailure =
    ErrorMessage "internal error" ["could not create listener socket"] Error
  toErrorMessage (InvalidShell command@(Cmd.Command _ ident _ _ _ _ _ _ _)) =
    ErrorMessage msg ["RunError.InvalidShell:", show command] Error
    where
      msg = "invalid command for shell: " <> show ident
  toErrorMessage (InvalidCmdline err) =
    ErrorMessage msg [msg] Warn
    where
      msg =
        "invalid command line: " <> err
  toErrorMessage (Unsupported runner tpe) =
    ErrorMessage msg [msg] Warn
    where
      msg =
        "runner `" <> runner <> "` does not support " <> tpe <> " commands"
  toErrorMessage (VimTest e) =
    ErrorMessage "vim-test failed" ["RunError.VimTest:", e] Error
  toErrorMessage NoLinesSpecified =
    ErrorMessage "no lines specified for command" ["RunError.NoLinesSpecified"] Warn
  toErrorMessage (SubprocFailed err) =
    ErrorMessage ("subprocess failed" <> userErr err) ("RunError.SubprocFailed" : err) Warn
    where
      userErr [] = ""
      userErr (e : _) = ": " <> e
