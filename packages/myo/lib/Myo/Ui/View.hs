module Myo.Ui.View where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError (..))
import Chiasma.Ui.Data.View (
  LayoutView,
  Pane (Pane),
  PaneView,
  Tree (..),
  TreeSub (..),
  View (View),
  ViewTree,
  consLayout,
  )
import qualified Chiasma.Ui.Data.View as View (_ident)
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Lens (mapMOf, transformM)
import Data.Generics.Labels ()

import qualified Myo.Data.ViewError as ViewError
import Myo.Data.ViewError (ViewError)
import Myo.Ui.Data.Space (Space (Space))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.ViewCoords (ViewCoords (ViewCoords))
import Myo.Ui.Data.Window (Window (Window))

insertSpace ::
  Member (AtomicState UiState) r =>
  Space ->
  Sem r ()
insertSpace space =
  atomicModify' (#spaces <>~ [space])

createSpace :: Member (AtomicState UiState) r => Ident -> Sem r Space
createSpace ident =
  space <$ insertSpace space
  where
    space = Space ident []

insertWindowIntoSpace :: Window -> Space -> Space
insertWindowIntoSpace w (Space i wins) = Space i (w:wins)

spaceLens :: Ident -> Traversal' [Space] Space
spaceLens =
  matchIdentL

spaceWindowLens :: Ident -> Ident -> Traversal' [Space] Window
spaceWindowLens spaceIdent windowIdent =
  spaceLens spaceIdent . #windows . matchIdentL windowIdent

uiSpaceLens :: Ident -> Traversal' UiState Space
uiSpaceLens ident =
  #spaces . spaceLens ident

doesSpaceExist :: Ident -> UiState -> Bool
doesSpaceExist i =
  has (uiSpaceLens i)

createWindow ::
  Member (AtomicState UiState) r =>
  ViewCoords ->
  Sem r (Maybe Window)
createWindow (ViewCoords spaceIdent windowIdent layoutIdent) = do
  atomicState' \ s ->
    if doesSpaceExist spaceIdent s
    then (s & uiSpaceLens spaceIdent %~ insertWindowIntoSpace window, Just window)
    else (s, Nothing)
  where
    window =
      Window windowIdent (Tree rootLayout [])
    rootLayout =
      consLayout layoutIdent

windowLayoutLens :: Ident -> Traversal' [Window] ViewTree
windowLayoutLens ident = matchIdentL ident . #layout

spacesLayoutLens :: Ident -> Ident -> Traversal' [Space] ViewTree
spacesLayoutLens spaceIdent windowIdent = spaceWindowLens spaceIdent windowIdent . #layout

insertLayoutIfNonexistent :: Ident -> LayoutView -> ViewTree -> Maybe ViewTree
insertLayoutIfNonexistent _ (View newIdent _ _ _) (Tree (View currentIdent _ _ _) _) | newIdent == currentIdent =
  Nothing
insertLayoutIfNonexistent targetLayoutIdent newLayout (Tree layout sub) =
  let newSub = if View._ident layout == targetLayoutIdent then TreeNode (Tree newLayout []) : sub else sub
  in Just (Tree layout newSub)

insertPaneIfNonexistent :: Ident -> PaneView -> ViewTree -> Maybe ViewTree
insertPaneIfNonexistent targetLayout pane (Tree layout@(View currentLayout _ _ _) sub) = do
  traverse_ match sub
  let newSub = if currentLayout == targetLayout then TreeLeaf pane : sub else sub
  pure (Tree layout newSub)
  where
    paneIdent = View._ident pane
    match (TreeLeaf (View i _ _ _)) = if paneIdent == i then Nothing else Just ()
    match _ = Just ()

uiTreeLens :: ViewCoords -> Traversal' UiState ViewTree
uiTreeLens (ViewCoords spaceIdent windowIdent _) =
  #spaces . spacesLayoutLens spaceIdent windowIdent

uiTreesLens :: Traversal' UiState ViewTree
uiTreesLens =
  #spaces . each . #windows . each . #layout

insertViewEnv ::
  (Ident -> View a -> ViewTree -> Maybe ViewTree) ->
  ViewCoords ->
  View a ->
  UiState ->
  Maybe UiState
insertViewEnv insert coords@(ViewCoords _ _ layoutIdent) view =
  mapMOf (uiTreeLens coords) (transformM $ insert layoutIdent view)

insertLayoutEnv :: ViewCoords -> LayoutView -> UiState -> Maybe UiState
insertLayoutEnv =
  insertViewEnv insertLayoutIfNonexistent

insertPaneEnv :: ViewCoords -> PaneView -> UiState -> Maybe UiState
insertPaneEnv =
  insertViewEnv insertPaneIfNonexistent

modifyTree ::
  Members [AtomicState UiState, Stop ViewError] r =>
  (UiState -> Either TreeModError UiState) ->
  Sem r ()
modifyTree f =
  stopEitherWith ViewError.TreeMod =<< atomicState' \ s -> case f s of
    Right a -> (a, Right ())
    Left e -> (s, Left e)

modifyTreeMaybe ::
  Members [AtomicState UiState, Stop ViewError] r =>
  (UiState -> Maybe UiState) ->
  TreeModError ->
  Sem r ()
modifyTreeMaybe trans err =
  modifyTree (maybe (Left err) Right . trans)

insertLayout ::
  Members [AtomicState UiState, Stop ViewError] r =>
  ViewCoords ->
  LayoutView ->
  Sem r ()
insertLayout coords view =
  modifyTreeMaybe (insertLayoutEnv coords view) (TreeModError.LayoutExists view)

insertPane ::
  Members [AtomicState UiState, Stop ViewError] r =>
  ViewCoords ->
  PaneView ->
  Sem r ()
insertPane coords view =
  modifyTreeMaybe (insertPaneEnv coords view) (TreeModError.PaneExists view)

paneToggleOpen :: PaneView -> PaneView
paneToggleOpen (View i s g (Pane False pin cwd)) =
  View i s g (Pane True pin cwd)
paneToggleOpen (View i (ViewState m) g e) =
  View i (ViewState (not m)) g e
