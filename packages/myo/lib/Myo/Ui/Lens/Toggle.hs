module Myo.Ui.Lens.Toggle where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError (..))
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (
  ToggleResult,
  ensurePaneOpenTraversal',
  toggleLayoutOpenTraversal',
  togglePaneOpenTraversal',
  )
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult (..))
import Control.Lens (Traversal')

import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.View (uiTreesLens)

liftError ::
  (Ident -> TreeModError) ->
  (Ident -> Int -> TreeModError) ->
  Ident ->
  ToggleResult UiState ->
  Either TreeModError UiState
liftError missing ambiguous ident =
  lift'
  where
    lift' (ToggleResult.Success a) =
      Right a
    lift' ToggleResult.NotFound =
      Left (missing ident)
    lift' (ToggleResult.Ambiguous n) =
      Left (ambiguous ident n)

liftPaneError ::
  Ident ->
  ToggleResult UiState ->
  Either TreeModError UiState
liftPaneError =
  liftError TreeModError.PaneMissing TreeModError.AmbiguousPane

liftLayoutError ::
  Ident ->
  ToggleResult UiState ->
  Either TreeModError UiState
liftLayoutError =
  liftError TreeModError.LayoutMissing TreeModError.AmbiguousLayout

toggleOne ::
  Members [AtomicState UiState, Stop TreeModError] r =>
  (Ident -> ToggleResult UiState -> Either TreeModError UiState) ->
  (Traversal' UiState ViewTree -> Ident -> UiState -> ToggleResult UiState) ->
  Ident ->
  Sem r ()
toggleOne err trans ident =
  stopEither =<< atomicState' \ s ->
    case err ident (trans uiTreesLens ident s) of
      Right a -> (a, Right ())
      Left e -> (s, Left e)

toggleOnePane ::
  Members [AtomicState UiState, Stop TreeModError] r =>
  Ident ->
  Sem r ()
toggleOnePane =
  toggleOne liftPaneError togglePaneOpenTraversal'

openOnePane ::
  Members [AtomicState UiState, Stop TreeModError] r =>
  Ident ->
  Sem r ()
openOnePane =
  toggleOne liftPaneError ensurePaneOpenTraversal'

toggleOneLayout ::
  Members [AtomicState UiState, Stop TreeModError] r =>
  Ident ->
  Sem r ()
toggleOneLayout =
  toggleOne liftLayoutError toggleLayoutOpenTraversal'
