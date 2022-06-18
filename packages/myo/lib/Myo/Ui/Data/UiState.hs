module Myo.Ui.Data.UiState where

import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.Views (Views)

import Myo.Ui.Data.Space (Space)

data UiState =
  UiState {
    spaces :: [Space],
    views :: Views,
    vimPaneId :: Maybe PaneId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)
