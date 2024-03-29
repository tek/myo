module Myo.Command.Data.CommandError where

import Exon (exon)
import Log (Severity (Error, Info, Warn))
import Ribosome (Report (Report), Reportable (toReport))

import Myo.Command.Data.Command (shortIdent)
import Myo.Data.CommandId (CommandId, commandIdText)
import qualified Myo.Data.CommandQuery as CommandQuery
import Myo.Data.CommandQuery (CommandQuery)

data CommandError =
  User Text
  |
  Misc Text
  |
  NoSuchCommand CommandQuery
  |
  NoCommands
  |
  NoSuchHistoryIndex Int
  |
  NoHistory
  |
  NotAUiShell CommandId CommandId
  |
  InvalidTemplate Bool Text Text
  deriving stock (Eq, Show)

instance Reportable CommandError where
  toReport (User err) =
    Report err ["CommandError.User:", err] Error
  toReport (Misc err) =
    Report (pre <> " " <> err) [pre, err] Error
    where
      pre = "command error:"
  toReport (NoSuchCommand query) =
    Report err ["CommandError.NoSuchCommand:", err] Warn
    where
      err = [exon|No match for #{CommandQuery.describe query}|]
  toReport NoCommands =
    Report err [err] Warn
    where
      err = "no commands have been created yet"
  toReport (NoSuchHistoryIndex index) =
    Report err [err] Warn
    where
      err = "no history entry at index " <> show index
  toReport NoHistory =
    Report err ["CommandError.NoHistory"] Info
    where
      err = "no history yet"
  toReport (NotAUiShell cmd shell) =
    Report msg log Error
    where
      msg =
        [exon|'#{shortIdent shell}' cannot be used as a shell for '#{shortIdent cmd}'|]
      log =
        ["CommandError.NotAUiShell:", [exon|cmd: #{commandIdText cmd}|], [exon|shell: #{commandIdText shell}|]]
  toReport (InvalidTemplate edit extra err) =
    Report msg log Error
    where
      msg = [exon|Parse error in#{new} command template: #{err}|]
      new | edit = " new"
          | otherwise = ""
      log = ["CommandError.InvalidTemplate:", err, extra]
