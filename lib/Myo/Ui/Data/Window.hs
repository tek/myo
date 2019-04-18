{-# LANGUAGE TemplateHaskell #-}

module Myo.Ui.Data.Window where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.View (ViewTree)
import Control.Lens (makeClassy)

data Window =
  Window {
    _ident :: Ident,
    _layout :: ViewTree
  }
  deriving (Eq, Show)

makeClassy ''Window

instance Identifiable Window where
  identify = _ident
