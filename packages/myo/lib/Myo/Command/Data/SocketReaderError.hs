module Myo.Command.Data.SocketReaderError where

import Chiasma.Data.Ident (Ident, identText)
import Exon (exon)
import Log (Severity (Warn))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data SocketReaderError =
  BindFailed Text
  |
  InvalidIdent Ident
  deriving stock (Eq, Show)

instance ToErrorMessage SocketReaderError where
  toErrorMessage = \case
    BindFailed err ->
      let msg = "Can't create command output socket. Check permissions of /tmp!"
      in ErrorMessage msg ["SocketReaderError.BindFailed:", err] Warn
    InvalidIdent (identText -> ident) ->
      let msg = [exon|Can't use command name `#{ident}` as socket path.|]
      in ErrorMessage msg ["SocketReaderError.InvalidIdent:", ident] Warn
