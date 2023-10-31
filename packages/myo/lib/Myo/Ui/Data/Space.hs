module Myo.Ui.Data.Space where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import Data.Data (Data)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))

import Myo.Ui.Data.Window (Window)

data Space =
  Space {
    ident :: Ident,
    windows :: [Window]
  }
  deriving stock (Eq, Show, Generic, Data)

instance Identifiable Space where
  identify = (.ident)

instance Pretty Space where
  pretty Space {..} =
    nest 2 . vsep $ header : (pretty <$> windows)
    where
      header = "⬚" <+> pretty ident
