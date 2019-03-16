module Myo.Ui.Default(
  setupDefaultUi,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(..), SessionId(..), WindowId(..))
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Ui.Data.View (
  Pane(..),
  Tree(..),
  TreeSub(..),
  View(..),
  ViewTree,
  consLayout,
  consLayoutVertical,
  consPane,
  )
import qualified Control.Lens as Lens (over)
import Control.Monad.State.Class (MonadState, modify)
import Myo.Data.Env (Env)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.View (envViewsLens, insertSpace)

insertInitialViews :: Views -> Views
insertInitialViews (Views sessions windows panes viewsLog) =
  Views
    (Tmux.View (Ident.Str "vim") (Just (SessionId 0)) : sessions)
    (Tmux.View (Ident.Str "vim") (Just (WindowId 0)) : windows)
    (Tmux.View (Ident.Str "vim") (Just (PaneId 0)) : panes)
    viewsLog

vimTree :: ViewTree
vimTree =
  Tree (consLayoutVertical (Ident.Str "vim")) [TreeLeaf ((consPane (Ident.Str "vim")) { extra = Pane True False Nothing })]

makeTree :: ViewTree
makeTree =
  Tree (consLayoutVertical (Ident.Str "make")) [TreeLeaf ((consPane (Ident.Str "make")) { extra = Pane False False Nothing })]

mainTree :: ViewTree
mainTree =
  Tree (consLayout (Ident.Str "main")) [TreeNode vimTree, TreeNode makeTree]

setupDefaultUi ::
  MonadState Env m =>
  m ()
setupDefaultUi = do
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") mainTree]
  modify $ Lens.over envViewsLens insertInitialViews
