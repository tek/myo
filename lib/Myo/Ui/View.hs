{-# LANGUAGE RankNTypes #-}

module Myo.Ui.View where

import Chiasma.Data.Ident (Ident, sameIdent)
import Chiasma.Data.Views (Views)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (
  LayoutView,
  Pane(Pane),
  PaneView,
  Tree(..),
  TreeSub(..),
  View(View),
  ViewTree,
  consLayout,
  viewIdent,
  )
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Lens (Lens', Traversal', each, has, mapMOf, transformM)
import qualified Control.Lens as Lens (view)
import Control.Lens.Setter ((%~), (<>~))
import Control.Monad ((<=<))
import Control.Monad.DeepError (MonadDeepError, hoistEither)
import Control.Monad.DeepState (MonadDeepState(put), gets, modify)
import Data.Foldable (traverse_)

import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (ui)
import Myo.Ui.Data.Space (Space(Space))
import qualified Myo.Ui.Data.Space as Space (windows)
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (spaces, views)
import Myo.Ui.Data.ViewCoords (ViewCoords(ViewCoords))
import Myo.Ui.Data.Window (Window(Window))
import qualified Myo.Ui.Data.Window as Window (layout)

envSpacesLens :: Lens' Env [Space]
envSpacesLens = Env.ui . UiState.spaces

envViewsLens :: Lens' Env Views
envViewsLens = Env.ui . UiState.views

insertSpace :: ∀ s m. MonadDeepState s UiState m => Space -> m ()
insertSpace space =
  modify @UiState $ UiState.spaces <>~ [space]

createSpace :: MonadDeepState s UiState m => Ident -> m Space
createSpace ident = do
  insertSpace space
  return space
  where
    space = Space ident []

insertWindowIntoSpace :: Window -> Space -> Space
insertWindowIntoSpace w (Space i wins) = Space i (w:wins)

spaceLens :: Ident -> Traversal' [Space] Space
spaceLens = matchIdentL

spaceWindowLens :: Ident -> Ident -> Traversal' [Space] Window
spaceWindowLens spaceIdent windowIdent = spaceLens spaceIdent . Space.windows . matchIdentL windowIdent

envSpaceLens :: Ident -> Traversal' Env Space
envSpaceLens ident = envSpacesLens . spaceLens ident

doesSpaceExist :: Ident -> Env -> Bool
doesSpaceExist = has . envSpaceLens

createWindow :: MonadDeepState s Env m => ViewCoords -> m (Maybe Window)
createWindow (ViewCoords spaceIdent windowIdent layoutIdent) = do
  exists <- gets $ doesSpaceExist spaceIdent
  modify $ envSpaceLens spaceIdent %~ insertWindowIntoSpace window
  return $ if exists then Just window else Nothing
  where
    window = Window windowIdent (Tree rootLayout [])
    rootLayout = consLayout layoutIdent

windowLayoutLens :: Ident -> Traversal' [Window] ViewTree
windowLayoutLens ident = matchIdentL ident . Window.layout

spacesLayoutLens :: Ident -> Ident -> Traversal' [Space] ViewTree
spacesLayoutLens spaceIdent windowIdent = spaceWindowLens spaceIdent windowIdent . Window.layout

insertLayoutIfNonexistent :: Ident -> LayoutView -> ViewTree -> Maybe ViewTree
insertLayoutIfNonexistent _ (View newIdent _ _ _) (Tree (View currentIdent _ _ _) _) | newIdent == currentIdent =
  Nothing
insertLayoutIfNonexistent targetLayoutIdent newLayout (Tree layout sub) =
  let newSub = if viewIdent layout == targetLayoutIdent then TreeNode (Tree newLayout []) : sub else sub
  in Just (Tree layout newSub)

insertPaneIfNonexistent :: Ident -> PaneView -> ViewTree -> Maybe ViewTree
insertPaneIfNonexistent targetLayout pane (Tree layout@(View currentLayout _ _ _) sub) = do
  traverse_ match sub
  let newSub = if currentLayout == targetLayout then TreeLeaf pane : sub else sub
  return (Tree layout newSub)
  where
    paneIdent = viewIdent pane
    match (TreeLeaf (View i _ _ _)) = if paneIdent == i then Nothing else Just ()
    match _ = Just ()

envTreeLens :: ViewCoords -> Traversal' Env ViewTree
envTreeLens (ViewCoords spaceIdent windowIdent _) =
  envSpacesLens . spacesLayoutLens spaceIdent windowIdent

envTreesLens :: Traversal' Env ViewTree
envTreesLens = envSpacesLens . each . Space.windows . each . Window.layout

uiTreesLens :: Traversal' UiState ViewTree
uiTreesLens = UiState.spaces . each . Space.windows . each . Window.layout

insertViewEnv ::
  (Ident -> View a -> ViewTree -> Maybe ViewTree) ->
  ViewCoords ->
  View a ->
  Env ->
  Maybe Env
insertViewEnv insert coords@(ViewCoords _ _ layoutIdent) view =
  mapMOf (envTreeLens coords) (transformM $ insert layoutIdent view)

insertLayoutEnv :: ViewCoords -> LayoutView -> Env -> Maybe Env
insertLayoutEnv =
  insertViewEnv insertLayoutIfNonexistent

insertPaneEnv :: ViewCoords -> PaneView -> Env -> Maybe Env
insertPaneEnv =
  insertViewEnv insertPaneIfNonexistent

modifyTree ::
  (MonadDeepError e TreeModError m, MonadDeepState s Env m) =>
  (Env -> Either TreeModError Env) ->
  m ()
modifyTree =
  put <=< hoistEither <=< gets

modifyTreeMaybe ::
  (MonadDeepError e TreeModError m, MonadDeepState s Env m) =>
  (Env -> Maybe Env) ->
  TreeModError ->
  m ()
modifyTreeMaybe trans err =
  modifyTree $ maybe (Left err) Right . trans

insertLayout ::
  (MonadDeepError e TreeModError m, MonadDeepState s Env m) =>
  ViewCoords ->
  LayoutView ->
  m ()
insertLayout coords view =
  modifyTreeMaybe (insertLayoutEnv coords view) (TreeModError.LayoutExists view)

insertPane ::
  (MonadDeepError e TreeModError m, MonadDeepState s Env m) =>
  ViewCoords ->
  PaneView ->
  m ()
insertPane coords view =
  modifyTreeMaybe (insertPaneEnv coords view) (TreeModError.PaneExists view)

paneToggleOpen :: PaneView -> PaneView
paneToggleOpen (View i s g (Pane False pin cwd)) =
  View i s g (Pane True pin cwd)
paneToggleOpen (View i (ViewState m) g e) =
  View i (ViewState (not m)) g e
