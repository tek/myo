module Myo.Ui.Data.ToggleError where

import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Ui.Data.TreeModError (TreeModError)

data ToggleError =
  Tmux TmuxError
  |
  Render RenderError
  |
  Tree TreeModError
  deriving (Eq, Show, Generic, ReportError)

deepPrisms ''ToggleError
