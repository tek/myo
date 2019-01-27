module Myo.Ui.Data.ViewCoords(
  ViewCoords(..),
  viewCoords,
) where

import Chiasma.Data.Ident (Ident(Str))

data ViewCoords =
  ViewCoords {
    vcSpace :: Ident,
    vcWindow :: Ident,
    vcLayout :: Ident
  }

viewCoords :: String -> String -> String -> ViewCoords
viewCoords si wi vi = ViewCoords (Str si) (Str wi) (Str vi)
