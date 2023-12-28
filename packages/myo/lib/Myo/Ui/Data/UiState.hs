module Myo.Ui.Data.UiState where

import Chiasma.Data.TmuxId (PaneId)
import Data.Data (Data)

import Myo.Ui.Data.Space (Space)

data UiState =
  UiState {
    spaces :: [Space],
    vimPaneId :: Maybe PaneId
  }
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (Default)
