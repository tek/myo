module Myo.Ui.Default(
  setupDefaultUi,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Data.TmuxId (SessionId(..), WindowId(..), PaneId(..))
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Ui.Data.View (Tree(..), TreeSub(..), ViewTree, View(..), Layout(..), Pane(..), consLayout, consPane)
import qualified Control.Lens as Lens (over)
import qualified Ribosome.Control.Ribo as Ribo (modify)

import Myo.Data.Env (Myo)
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
  Tree (consLayout (Ident.Str "vim")) [TreeLeaf ((consPane (Ident.Str "vim")) { extra = Pane True False Nothing })]

makeTree :: ViewTree
makeTree =
  Tree (consLayout (Ident.Str "make")) [TreeLeaf ((consPane (Ident.Str "make")) { extra = Pane False False Nothing })]

mainTree :: ViewTree
mainTree =
  Tree ((consLayout (Ident.Str "main")) { extra = Layout True }) [TreeNode vimTree, TreeNode makeTree]

setupDefaultUi :: Myo ()
setupDefaultUi = do
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") mainTree]
  Ribo.modify $ Lens.over envViewsLens insertInitialViews
