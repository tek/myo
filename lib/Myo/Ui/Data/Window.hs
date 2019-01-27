{-# LANGUAGE TemplateHaskell #-}

module Myo.Ui.Data.Window(
  Window(..),
  _layout,
) where

import Control.Lens (makeClassy_)
import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Ui.Data.View (ViewTree)

data Window =
  Window {
    ident :: Ident,
    layout :: ViewTree
  }
  deriving (Eq, Show)

makeClassy_ ''Window

instance Identifiable Window where
  identify = ident
