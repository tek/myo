module Myo.Ui.Default where

import Chiasma.Command.Pane (PanePidCodec(PanePidCodec), panePids)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(..), SessionId(..), WindowId(..))
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Monad.Stream (TmuxProg)
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
import Control.Lens (findMOf)
import qualified Control.Lens as Lens (each, over)
import Control.Monad.DeepState (MonadDeepState, modify)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Ribosome.Api.Process (vimPid)
import Ribosome.Control.Monad.Ribo (Nvim)
import Ribosome.Data.Functor ((<$<))
import Ribosome.Nvim.Api.RpcCall (RpcError)

import Myo.Data.Env (Env)
import Myo.System.Proc (ppids)
import Myo.Tmux.IO (RunTmux, runMyoTmux)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.UiState (UiState)
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

setupDefaultTestUi ::
  (MonadDeepState s Env m, MonadDeepState s UiState m) =>
  m ()
setupDefaultTestUi = do
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") mainTree]
  modify $ Lens.over envViewsLens insertInitialViews

containsVimPid :: MonadIO m => PanePidCodec -> Int -> m Bool
containsVimPid (PanePidCodec _ panePid) =
  (panePid `elem`) <$< ppids

detectVimPidPane ::
  (MonadIO m, Nvim m) =>
  Int ->
  TmuxProg m (Either () (SessionId, WindowId, PaneId))
detectVimPidPane vpid = do
  mainPids <- panePids
  _ <- lift $ findMOf Lens.each (`containsVimPid` vpid) mainPids
  undefined

detectVimPane ::
  (MonadIO m, Nvim m) =>
  TmuxProg m (Either () (SessionId, WindowId, PaneId))
detectVimPane =
  either (const (return (Left ()))) detectVimPidPane =<< runExceptT @RpcError vimPid

detectDefaultUi ::
  (MonadIO m, Nvim m, MonadDeepState s Views m, MonadDeepState s UiState m, RunTmux m) =>
  m ()
detectDefaultUi = do
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") mainTree]
  paneResult <- runMyoTmux detectVimPane
  case paneResult of
    Right _ -> undefined
    _ -> undefined
