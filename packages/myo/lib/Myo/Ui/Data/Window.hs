module Myo.Ui.Data.Window where

import Chiasma.Data.Ident (Identifiable (..))
import Chiasma.Ui.Data.View (ViewTree)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))

data Window =
  Window {
    _ident :: Ident,
    _layout :: ViewTree
  }
  deriving (Eq, Show)

makeClassy ''Window

instance Identifiable Window where
  identify = _ident

instance Pretty Window where
  pretty (Window ident' layout') =
    nest 2 . vsep $ [header, pretty layout']
    where
      header = "□" <+> pretty ident'
