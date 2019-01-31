module Myo.Command.Data.Command(
  Command(..),
) where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter)

data Command =
  Command {
    cmdInterpreter :: CommandInterpreter,
    cmdIdent :: Ident,
    cmdLines :: [String],
    cmdRunner :: Maybe Ident
  }
  deriving (Eq, Show)

instance Identifiable Command where
  identify = cmdIdent
