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
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry)
import Control.Lens (findMOf)
import qualified Control.Lens as Lens (each)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState, modify, setL)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Functor.Syntax ((<$$>))
import Ribosome.Api.Process (vimPid)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim, NvimE)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

import Myo.Orphans ()
import qualified Myo.Settings as Settings (vimPaneGeometry)
import Myo.System.Proc (ppids)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (vimPaneId)
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.View (insertSpace)

insertInitialViews :: SessionId -> WindowId -> PaneId -> Views -> Views
insertInitialViews sid wid pid (Views sessions windows panes viewsLog) =
  Views
    (Tmux.View (Ident.Str "vim") (Just sid) : sessions)
    (Tmux.View (Ident.Str "vim") (Just wid) : windows)
    (Tmux.View (Ident.Str "vim") (Just pid) : panes)
    viewsLog

vimTree :: ViewGeometry -> ViewTree
vimTree vimGeometry =
  Tree vimLayout [TreeLeaf vimPane]
  where
    vimLayout = (consLayoutVertical (Ident.Str "vim")) { _geometry = vimGeometry }
    vimPane = (consPane (Ident.Str "vim")) { _extra = Pane True False Nothing }

makeTree :: ViewTree
makeTree =
  Tree (consLayoutVertical (Ident.Str "make")) [
    TreeLeaf ((consPane (Ident.Str "make")) { _extra = Pane False True Nothing })
    ]

mainTree :: ViewGeometry -> ViewTree
mainTree vimGeometry =
  Tree (consLayout (Ident.Str "main")) [TreeNode (vimTree vimGeometry), TreeNode makeTree]

setupDefaultUi ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s UiState m =>
  m ()
setupDefaultUi = do
  vimGeometry <- setting Settings.vimPaneGeometry
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") (mainTree vimGeometry)]

setupDefaultTestUi ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Views m =>
  MonadDeepState s UiState m =>
  m ()
setupDefaultTestUi = do
  setupDefaultUi
  modify $ insertInitialViews (SessionId 0) (WindowId 0) (PaneId 0)

containsVimPid :: MonadIO m => Codec.PanePid -> Int -> m Bool
containsVimPid (Codec.PanePid _ panePid) =
  (panePid `elem`) <$$> ppids

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
  NvimE e m =>
  MonadRibo m =>
  MonadIO m =>
  Nvim m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Views m =>
  MonadDeepState s UiState m =>
  RunTmux m =>
  m ()
detectDefaultUi = do
  setupDefaultUi
  (Codec.PaneCoords sid wid pid) <- runRiboTmux detectVimPane
  modify $ insertInitialViews sid wid pid
  setL @UiState UiState.vimPaneId (Just pid)
