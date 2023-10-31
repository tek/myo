module Myo.Ui.Lens.Toggle where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Chiasma.Ui.Data.View (Pane (Pane), PaneView, Tree (Tree), View (View), ViewTree)
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import qualified Chiasma.Ui.ViewTree as ToggleResult
import Chiasma.Ui.ViewTree (
  ToggleResult,
  ToggleStatus (Consistent, Minimized, Pristine),
  checkToggleResult,
  depthTraverseTree,
  ensurePaneOpenTraversal',
  isOpenPaneNode,
  openFirstPaneNode,
  openFirstPinnedPaneNode,
  openPinnedSubs,
  skipFold,
  toggleLayoutOpenTraversal',
  togglePaneOpenTraversal',
  )
import Control.Lens (mapMOf)

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

ensureLayoutNodeOpen :: Ident -> ToggleStatus -> ViewTree -> (ToggleStatus, ViewTree)
ensureLayoutNodeOpen ident previous t@(Tree v@(View i (ViewState minimized) g l) sub)
  | ident == i
  , open
  , minimized
  = (Minimized, Tree (View i (ViewState False) g l) sub)
  | ident == i
  , not open
  = bimap (previous <>) (Tree v) (uncurry regularIfPristine openFirstPinned)
  | otherwise
  = (previous, t)
  where
    open = any isOpenPaneNode sub
    openFirstPinned =
      skipFold openFirstPinnedPaneNode Pristine sub
    openFirstRegular =
      skipFold openFirstPaneNode Pristine sub
    regularIfPristine Pristine _ =
      openFirstRegular
    regularIfPristine status a =
      (status, a)

ensureLayoutOpen :: Ident -> ViewTree -> ToggleResult ViewTree
ensureLayoutOpen ident =
  uncurry checkToggleResult .
  depthTraverseTree (\ a b -> uncurry openPinnedSubs (ensureLayoutNodeOpen ident a b)) (Pristine,)

ensureLayoutOpenTraversal' ::
  Traversal' a ViewTree ->
  Ident ->
  a ->
  ToggleResult a
ensureLayoutOpenTraversal' lens =
  mapMOf lens . ensureLayoutOpen

openOneLayout ::
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
openOneLayout i =
  mapStop ToggleError.Tree do
    toggleOne liftPaneError ensureLayoutOpenTraversal' i

ensurePaneViewHidden :: Ident -> PaneView -> (ToggleStatus, PaneView)
ensurePaneViewHidden ident (View i s g (Pane True p c)) | ident == i =
  (Minimized, View i s g (Pane False p c))
ensurePaneViewHidden ident v@(View i _ _ _) | ident == i =
  (Consistent, v)
ensurePaneViewHidden _ v =
  (Pristine, v)

hidePane :: Ident -> ViewTree -> ToggleResult ViewTree
hidePane ident =
  uncurry checkToggleResult . depthTraverseTree (,) (ensurePaneViewHidden ident)

hidePaneTraversal' ::
  Traversal' a ViewTree ->
  Ident ->
  a ->
  ToggleResult a
hidePaneTraversal' lens =
  mapMOf lens . hidePane

hideOnePane ::
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
hideOnePane i =
  mapStop ToggleError.Tree do
    toggleOne liftPaneError hidePaneTraversal' i

hideLayoutNode :: Ident -> ToggleStatus -> ViewTree -> (ToggleStatus, ViewTree)
hideLayoutNode ident previous (Tree (View i (ViewState False) g l) sub)
  | ident == i
  , any isOpenPaneNode sub
  =
  (previous <> Minimized, Tree (View i (ViewState True) g l) sub)
hideLayoutNode _ a t =
  (a, t)

hideLayout :: Ident -> ViewTree -> ToggleResult ViewTree
hideLayout ident =
  uncurry checkToggleResult . depthTraverseTree (hideLayoutNode ident) (Pristine,)

hideLayoutTraversal' ::
  Traversal' a ViewTree ->
  Ident ->
  a ->
  ToggleResult a
hideLayoutTraversal' lens =
  mapMOf lens . hideLayout

hideOneLayout ::
  Members [AtomicState UiState, Stop ToggleError] r =>
  Ident ->
  Sem r ()
hideOneLayout i =
  mapStop ToggleError.Tree do
    toggleOne liftLayoutError hideLayoutTraversal' i
