module Myo.Ui.Lens.Toggle where

import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (
  ToggleResult,
  ensurePaneOpenTraversal',
  toggleLayoutOpenTraversal',
  togglePaneOpenTraversal',
  )
import qualified Chiasma.Ui.ViewTree as ToggleResult (ToggleResult(..))
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
  MonadDeepState s UiState m =>
  MonadDeepError e TreeModError m =>
  (Ident -> ToggleResult UiState -> Either TreeModError UiState) ->
  (Traversal' UiState ViewTree -> Ident -> UiState -> ToggleResult UiState) ->
  Ident ->
  m ()
toggleOne err trans ident =
  modifyM $ hoistEither . err ident . trans uiTreesLens ident

toggleOnePane ::
  MonadDeepState s UiState m =>
  MonadDeepError e TreeModError m =>
  Ident ->
  m ()
toggleOnePane =
  toggleOne liftPaneError togglePaneOpenTraversal'

openOnePane ::
  MonadDeepState s UiState m =>
  MonadDeepError e TreeModError m =>
  Ident ->
  m ()
openOnePane =
  toggleOne liftPaneError ensurePaneOpenTraversal'

toggleOneLayout ::
  MonadDeepState s UiState m =>
  MonadDeepError e TreeModError m =>
  Ident ->
  m ()
toggleOneLayout =
  toggleOne liftLayoutError toggleLayoutOpenTraversal'
