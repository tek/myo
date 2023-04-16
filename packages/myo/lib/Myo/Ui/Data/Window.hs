module Myo.Ui.Data.Window where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import Chiasma.Ui.Data.View (ViewTree)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))

data Window =
  Window {
    ident :: Ident,
    layout :: ViewTree
  }
  deriving stock (Eq, Show, Generic)

instance Identifiable Window where
  identify = (.ident)

instance Pretty Window where
  pretty Window {..} =
    nest 2 (vsep [header, pretty layout])
    where
      header = "□" <+> pretty ident
