{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Myo.Ui.Data.UiState(
  UiState(..),
  _spaces,
  _views,
) where

import Control.Lens (makeClassy_)
import GHC.Generics (Generic)
import Data.Default (Default)
import Chiasma.Data.Views (Views)
import Myo.Ui.Data.Space (Space)

data UiState =
  UiState {
    spaces :: [Space],
    views :: Views
  }
  deriving (Eq, Show, Generic, Default)

makeClassy_ ''UiState
