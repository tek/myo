module Myo.Ui.Default where

import qualified Chiasma.Codec.Data.PaneCoords as Codec (PaneCoords(PaneCoords))
import qualified Chiasma.Codec.Data.PanePid as Codec (PanePid(PanePid))
import Chiasma.Command.Pane (pane, panePids)
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
import Control.Monad.Trans.Maybe (MaybeT(..))
import Ribosome.Api.Process (vimPid)
import Ribosome.Control.Monad.Ribo (Nvim)
import Ribosome.Data.Functor ((<$<))
import Ribosome.Nvim.Api.RpcCall (RpcError)

import Myo.Data.Env (Env)
import Myo.System.Proc (ppids)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.View (envViewsLens, insertSpace)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

insertInitialViews :: SessionId -> WindowId -> PaneId -> Views -> Views
insertInitialViews sid wid pid (Views sessions windows panes viewsLog) =
  Views
    (Tmux.View (Ident.Str "vim") (Just sid) : sessions)
    (Tmux.View (Ident.Str "vim") (Just wid) : windows)
    (Tmux.View (Ident.Str "vim") (Just pid) : panes)
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
  modify $ Lens.over envViewsLens (insertInitialViews (SessionId 0) (WindowId 0) (PaneId 0))

containsVimPid :: MonadIO m => Codec.PanePid -> Int -> m Bool
containsVimPid (Codec.PanePid _ panePid) =
  (panePid `elem`) <$< ppids

detectVimPidPane ::
  (MonadIO m, Nvim m) =>
  Int ->
  TmuxProg m Codec.PaneCoords
detectVimPidPane vpid = do
  mainPids <- panePids
  result <- runMaybeT (findId mainPids)
  maybe (fail "could not find vim pid in any tmux pane") return result
  where
    findId pids = do
      (Codec.PanePid paneId _) <- MaybeT $ lift (findMOf Lens.each (`containsVimPid` vpid) pids)
      MaybeT $ pane paneId

detectVimPane ::
  (MonadIO m, Nvim m) =>
  TmuxProg m Codec.PaneCoords
detectVimPane = do
  result <- runExceptT @RpcError vimPid
  either (fail . show) detectVimPidPane result

detectDefaultUi ::
  (MonadIO m, Nvim m, MonadDeepState s Views m, MonadDeepState s UiState m, RunTmux m) =>
  m ()
detectDefaultUi = do
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") mainTree]
  (Codec.PaneCoords sid wid pid) <- runRiboTmux detectVimPane
  modify $ insertInitialViews sid wid pid
