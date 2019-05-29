{-# LANGUAGE DeriveAnyClass #-}

module Myo.Ui.Data.UiState where

import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views)
import Data.DeepLenses (deepLenses)
import Data.Default (Default)
import GHC.Generics (Generic)

import Myo.Ui.Data.Space (Space)

data UiState =
  UiState {
    _spaces :: [Space],
    _views :: Views,
    _vimPaneId :: Maybe PaneId
  }
  deriving (Eq, Show, Generic, Default)

deepLenses ''UiState
