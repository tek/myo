module Myo.Ui.Lens.Toggle where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Chiasma.Ui.Data.View (ViewTree)
import qualified Chiasma.Ui.ViewTree as ToggleResult
import Chiasma.Ui.ViewTree (
  ToggleResult,
  ensurePaneOpenTraversal',
  toggleLayoutOpenTraversal',
  togglePaneOpenTraversal',
  )

import qualified Myo.Ui.Data.ToggleError as ToggleError
import Myo.Ui.Data.ToggleError (ToggleError)
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
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
toggleOnePane i =
  mapStop ToggleError.Tree do
    toggleOne liftPaneError togglePaneOpenTraversal' i

openOnePane ::
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
openOnePane i =
  mapStop ToggleError.Tree do
    toggleOne liftPaneError ensurePaneOpenTraversal' i

toggleOneLayout ::
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
toggleOneLayout i =
  mapStop ToggleError.Tree do
    toggleOne liftLayoutError toggleLayoutOpenTraversal' i
