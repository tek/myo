module Myo.Command.Data.AddSystemCommandOptions(
  AddSystemCommandOptions(..),
) where

import Chiasma.Data.Ident (Ident)

import Myo.Command.Data.Command (CommandLanguage)

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: Ident,
    lines :: [String],
    runner :: Maybe Ident,
    target :: Maybe Ident,
    _lines :: Maybe CommandLanguage
    }
