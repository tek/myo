
module Myo.Ui.Data.Space where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Control.Lens (makeClassy)
import Data.Text.Prettyprint.Doc (Pretty(..), nest, vsep, (<+>))

import Myo.Ui.Data.Window (Window)

data Space =
  Space {
    _ident :: Ident,
    _windows :: [Window]
  }
  deriving (Eq, Show)

makeClassy ''Space

instance Identifiable Space where
  identify = _ident

instance Pretty Space where
  pretty (Space ident' windows') =
    nest 2 . vsep $ header : (pretty <$> windows')
    where
      header = "⬚" <+> pretty ident'
