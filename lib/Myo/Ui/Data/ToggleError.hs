{-# LANGUAGE TemplateHaskell #-}

module Myo.Ui.Data.ToggleError(
  ToggleError(..),
) where

import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Error.Report.Class (ReportError(..))

import Myo.Ui.Error (renderErrorReport, tmuxErrorReport, treeModErrorReport)

data ToggleError =
  Tmux TmuxError
  |
  Render RenderError
  |
  Tree TreeModError
  deriving (Eq, Show)

deepPrisms ''ToggleError

instance ReportError ToggleError where
  errorReport (Tmux e) = tmuxErrorReport e
  errorReport (Render e) = renderErrorReport e
  errorReport (Tree e) = treeModErrorReport e
