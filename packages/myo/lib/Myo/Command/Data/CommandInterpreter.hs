module Myo.Command.Data.CommandInterpreter where

import Chiasma.Data.Ident (Ident)
import Prettyprinter (Pretty (..), emptyDoc, (<+>))

import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)
import Myo.Data.String.Pretty (backtick)
import Myo.Orphans ()

data CommandInterpreter =
  System {
    systemTarget :: Maybe UiTarget
  }
  |
  Shell {
    shellTarget :: CommandId
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
