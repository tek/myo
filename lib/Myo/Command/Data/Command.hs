module Myo.Command.Data.Command(
  Command(..),
) where

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)
import Chiasma.Data.Ident (Ident)

data Command =
  Command {
    cmdIdent :: Ident,
    cmdInterpreter :: CommandInterpreter
  }
  deriving (Eq, Show)
