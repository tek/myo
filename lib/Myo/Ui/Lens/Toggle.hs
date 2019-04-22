module Myo.Ui.Lens.Toggle where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.ViewTree (
  ToggleResult,
  ensurePaneOpenTraversal',
  toggleLayoutOpenTraversal',
  togglePaneOpenTraversal',
  )
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult(..))

import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.View (uiTreesLens)

convertError ::
  MonadDeepError e TreeModError m =>
  (Ident -> TreeModError) ->
  (Ident -> Int -> TreeModError) ->
  Ident ->
  ToggleResult UiState ->
  m UiState
convertError missing ambiguous ident =
  convert
  where
    convert (ToggleResult.Success a) =
      return a
    convert ToggleResult.NotFound =
      throwHoist (missing ident)
    convert (ToggleResult.Ambiguous n) =
      throwHoist (ambiguous ident n)

convertPaneError ::
  MonadDeepError e TreeModError m =>
  Ident ->
  ToggleResult UiState ->
  m UiState
convertPaneError =
  convertError TreeModError.PaneMissing TreeModError.AmbiguousPane

convertLayoutError ::
  MonadDeepError e TreeModError m =>
  Ident ->
  ToggleResult UiState ->
  m UiState
convertLayoutError =
  convertError TreeModError.LayoutMissing TreeModError.AmbiguousLayout

toggleOnePane ::
  MonadDeepError e TreeModError m =>
  Ident ->
  UiState ->
  m UiState
toggleOnePane ident =
  convertPaneError ident . togglePaneOpenTraversal' uiTreesLens ident

openOnePane :: MonadDeepError e TreeModError m => Ident -> UiState -> m UiState
openOnePane ident =
  convertPaneError ident . ensurePaneOpenTraversal' uiTreesLens ident

toggleOneLayout :: MonadDeepError e TreeModError m => Ident -> UiState -> m UiState
toggleOneLayout ident =
  convertLayoutError ident . toggleLayoutOpenTraversal' uiTreesLens ident
