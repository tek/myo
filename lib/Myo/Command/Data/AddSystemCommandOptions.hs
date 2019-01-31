module Myo.Command.Data.AddSystemCommandOptions(
  AddSystemCommandOptions(..),
) where

import Chiasma.Data.Ident (Ident)

data AddSystemCommandOptions =
  AddSystemCommandOptions {
    ident :: Ident,
    lines :: [String],
    runner :: Maybe Ident,
    target :: Maybe Ident
    }
