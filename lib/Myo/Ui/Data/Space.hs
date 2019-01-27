{-# LANGUAGE TemplateHaskell #-}

module Myo.Ui.Data.Space(
  Space(..),
  _windows,
) where

import Control.Lens (makeClassy_)
import Chiasma.Data.Ident (Ident, Identifiable(..))
import Myo.Ui.Data.Window (Window)

data Space =
  Space {
    ident :: Ident,
    windows :: [Window]
  }
  deriving (Eq, Show)

makeClassy_ ''Space

instance Identifiable Space where
  identify = ident
