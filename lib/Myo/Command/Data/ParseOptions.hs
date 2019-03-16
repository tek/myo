module Myo.Command.Data.ParseOptions(
  ParseOptions(..),
) where

import Chiasma.Data.Ident (Ident)
import Myo.Command.Data.Command (CommandLanguage)

data ParseOptions =
  ParseOptions {
    pane :: Maybe Ident,
    command :: Maybe Ident,
    lang :: Maybe CommandLanguage
  }
  deriving (Eq, Show)
