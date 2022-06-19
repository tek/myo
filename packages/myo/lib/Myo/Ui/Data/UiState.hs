module Myo.Ui.Data.UiState where

import Chiasma.Data.TmuxId (PaneId)

import Myo.Ui.Data.Space (Space)

data UiState =
  UiState {
    spaces :: [Space],
    vimPaneId :: Maybe PaneId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
