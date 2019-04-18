{-# LANGUAGE TemplateHaskell #-}

module Myo.Ui.Data.Space where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Control.Lens (makeClassy)
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
