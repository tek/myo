module Myo.Command.Data.Command(
  Command(..),
  CommandLanguage(..),
) where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter)

newtype CommandLanguage =
  CommandLanguage String
  deriving (Eq, Show, Ord)

data Command =
  Command {
    cmdInterpreter :: CommandInterpreter,
    cmdIdent :: Ident,
    cmdLines :: [String],
    cmdRunner :: Maybe Ident,
    lang :: Maybe CommandLanguage
  }
  deriving (Eq, Show)

instance Identifiable Command where
  identify = cmdIdent
