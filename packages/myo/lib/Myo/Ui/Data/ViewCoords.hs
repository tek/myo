module Myo.Ui.Data.ViewCoords where

import Chiasma.Data.Ident (Ident(Str))

data ViewCoords =
  ViewCoords {
    vcSpace :: Ident,
    vcWindow :: Ident,
    vcLayout :: Ident
  }

viewCoords :: Text -> Text -> Text -> ViewCoords
viewCoords si wi vi = ViewCoords (Str si) (Str wi) (Str vi)
