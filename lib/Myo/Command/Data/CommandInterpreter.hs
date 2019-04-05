module Myo.Command.Data.CommandInterpreter where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Text.Pretty (prettyS)
import Data.Text.Prettyprint.Doc (Pretty(..), emptyDoc, (<+>))

import Myo.Data.String.Pretty (backtick)

data CommandInterpreter =
  System {
    systemTarget :: Maybe Ident
  }
  |
  Shell {
    shellTarget :: Ident
  }
  |
  Vim {
    vimSilent :: Bool,
    vimTarget :: Maybe Ident
  }
  deriving (Eq, Show)

instance Pretty CommandInterpreter where
  pretty (System target) =
    prettyS "system" <+> prettyTarget target
    where
      prettyTarget (Just a) =
        prettyS "in pane" <+> backtick (pretty a)
      prettyTarget Nothing =
        emptyDoc
  pretty (Shell target) =
    prettyS "shell" <+> backtick (pretty target)
  pretty (Vim silent target) =
    prettyS "vim" <+> prettyTarget target <+> prettySilent silent
    where
      prettyTarget (Just a) =
        prettyS "in pane" <+> backtick (pretty a)
      prettyTarget Nothing =
        emptyDoc
      prettySilent True =
        prettyS "(silent)"
      prettySilent False =
        emptyDoc
