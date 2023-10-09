module Myo.Command.Data.SocketReaderError where

import Exon (exon)
import Log (Severity (Warn))
import Ribosome (Report (Report), Reportable (toReport))

import Myo.Data.CommandId (CommandId, commandIdText)

data SocketReaderError =
  BindFailed Text
  |
  InvalidIdent CommandId
  deriving stock (Eq, Show)

instance Reportable SocketReaderError where
  toReport = \case
    BindFailed err ->
      let msg = "Can't create command output socket. Check permissions of /tmp!"
      in Report msg ["SocketReaderError.BindFailed:", err] Warn
    InvalidIdent (commandIdText -> ident) ->
      let msg = [exon|Can't use command name '#{ident}' as socket path.|]
      in Report msg ["SocketReaderError.InvalidIdent:", ident] Warn
