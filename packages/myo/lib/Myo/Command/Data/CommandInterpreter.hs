module Myo.Command.Data.CommandInterpreter where

import Chiasma.Data.Ident (Ident)
import Polysemy.Time.Json (json)
import Prettyprinter (Pretty (..), emptyDoc, (<+>))

import Myo.Data.String.Pretty (backtick)
import Myo.Orphans ()

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
  deriving stock (Eq, Show, Generic)

json ''CommandInterpreter

instance Pretty CommandInterpreter where
  pretty (System target) =
    "system" <+> prettyTarget target
    where
      prettyTarget (Just a) =
        "in pane" <+> backtick (pretty a)
      prettyTarget Nothing =
        emptyDoc
  pretty (Shell target) =
    "shell" <+> backtick (pretty target)
  pretty (Vim silent target) =
    "vim" <+> prettyTarget target <+> prettySilent silent
    where
      prettyTarget (Just a) =
        "in pane" <+> backtick (pretty a)
      prettyTarget Nothing =
        emptyDoc
      prettySilent True =
        "(silent)"
      prettySilent False =
        emptyDoc
