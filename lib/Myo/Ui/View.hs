{-# LANGUAGE RankNTypes #-}

module Myo.Ui.View where

import Control.Lens (Lens', Traversal', transformM, mapMOf, has, each)
import Control.Lens.Setter ((<>~), (%~))
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Control.Monad.Trans.Class (lift)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Views (Views)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Chiasma.Ui.Data.View (
  View(View),
  consLayout,
  Pane(Pane),
  Tree(..),
  ViewTree,
  LayoutView,
  PaneView,
  TreeSub(..),
  viewIdent,
  )
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
import qualified Ribosome.Control.Ribo as Ribo (modify, inspect, put)
import qualified Myo.Data.Env as Env (_ui)
import Myo.Data.Env (Myo, Ribo, Env)
import Myo.Ui.Data.ViewCoords (ViewCoords(ViewCoords))
import Myo.Ui.Data.Space (Space(Space))
import qualified Myo.Ui.Data.Space as Space (_windows)
import Myo.Ui.Data.Window (Window(Window))
import qualified Myo.Ui.Data.Window as Window (_layout)
import qualified Myo.Ui.Data.UiState as UiState (_spaces, _views)
import Data.Foldable (traverse_)

envSpacesLens :: Lens' Env [Space]
envSpacesLens = Env._ui . UiState._spaces

envViewsLens :: Lens' Env Views
envViewsLens = Env._ui . UiState._views

insertSpace :: Space -> Myo ()
insertSpace space =
  Ribo.modify $ envSpacesLens <>~ [space]

createSpace :: Ident -> Myo Space
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
spaceWindowLens spaceIdent windowIdent = spaceLens spaceIdent . Space._windows . matchIdentL windowIdent

envSpaceLens :: Ident -> Traversal' Env Space
envSpaceLens ident = envSpacesLens . spaceLens ident

doesSpaceExist :: Ident -> Env -> Bool
doesSpaceExist = has . envSpaceLens

createWindow :: ViewCoords -> Myo (Maybe Window)
createWindow (ViewCoords spaceIdent windowIdent layoutIdent) = do
  exists <- Ribo.inspect $ doesSpaceExist spaceIdent
  Ribo.modify $ envSpaceLens spaceIdent %~ insertWindowIntoSpace window
  return $ if exists then Just window else Nothing
  where
    window = Window windowIdent (Tree rootLayout [])
    rootLayout = consLayout layoutIdent

windowLayoutLens :: Ident -> Traversal' [Window] ViewTree
windowLayoutLens ident = matchIdentL ident . Window._layout

spacesLayoutLens :: Ident -> Ident -> Traversal' [Space] ViewTree
spacesLayoutLens spaceIdent windowIdent = spaceWindowLens spaceIdent windowIdent . Window._layout

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
envTreesLens = envSpacesLens . each . Space._windows . each . Window._layout

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

modifyTree :: (Env -> Either TreeModError Env) -> ExceptT TreeModError (Ribo Env) ()
modifyTree trans = do
  env <- ExceptT $ Ribo.inspect trans
  lift $ Ribo.put env

modifyTreeMaybe :: (Env -> Maybe Env) -> TreeModError -> ExceptT TreeModError (Ribo Env) ()
modifyTreeMaybe trans err =
  modifyTree $ maybe (Left err) Right . trans

insertLayout :: ViewCoords -> LayoutView -> ExceptT TreeModError (Ribo Env) ()
insertLayout coords view =
  modifyTreeMaybe (insertLayoutEnv coords view) (TreeModError.LayoutExists view)

insertPane :: ViewCoords -> PaneView -> ExceptT TreeModError (Ribo Env) ()
insertPane coords view =
  modifyTreeMaybe (insertPaneEnv coords view) (TreeModError.PaneExists view)

paneToggleOpen :: PaneView -> PaneView
paneToggleOpen (View i s g (Pane False pin cwd)) =
  View i s g (Pane True pin cwd)
paneToggleOpen (View i (ViewState m) g e) =
  View i (ViewState (not m)) g e
