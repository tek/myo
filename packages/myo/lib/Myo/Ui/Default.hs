module Myo.Ui.Default where

import qualified Chiasma.Codec.Data.PaneCoords as Codec
import Chiasma.Codec.Data.PaneCoords (PaneCoords)
import qualified Chiasma.Codec.Data.PanePid as Codec
import Chiasma.Codec.Data.PanePid (PanePid)
import Chiasma.Command.Pane (pane, panes)
import qualified Chiasma.Data.Ident as Ident (Ident (Str))
import Chiasma.Data.Panes (Panes, TmuxPanes)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId (..), SessionId (..), WindowId (..))
import qualified Chiasma.Data.View as Tmux (View (View))
import Chiasma.Data.Views (Views (Views))
import Chiasma.Effect.Codec (NativeCodecsE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.TmuxNative (withTmuxApisNative_)
import Chiasma.Ui.Data.View (
  Pane (..),
  Tree (..),
  TreeSub (..),
  View (..),
  ViewTree,
  consLayout,
  consLayoutVertical,
  consPane,
  )
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry))
import Control.Monad.Extra (findM)
import Process (Pid (Pid))
import Ribosome (Rpc, RpcError, Settings, mapUserMessage, resumeHoistUserMessage)
import Ribosome.Api (vimPid)
import qualified Ribosome.Settings as Settings

import Myo.Data.ProcError (ProcError)
import qualified Myo.Data.ViewError as ViewError
import Myo.Data.ViewError (ViewError)
import qualified Myo.Effect.Proc as Proc
import Myo.Effect.Proc (Proc)
import Myo.Orphans ()
import qualified Myo.Settings as Settings
-- import Myo.System.Proc (ppids)
import Myo.Ui.Data.DetectUiError (DetectUiError (Unexpected, VimPaneNotFound))
import Myo.Ui.Data.Space (Space (Space))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.Window (Window (Window))
import Myo.Ui.View (insertSpace)

scratchGeometry :: ViewGeometry
scratchGeometry =
  ViewGeometry Nothing (Just 20) Nothing Nothing (Just 0.3) Nothing

insertInitialViews :: SessionId -> WindowId -> PaneId -> Views -> Views
insertInitialViews sid wid pid (Views sessions windows ps viewsLog) =
  Views
    (Tmux.View (Ident.Str "vim") (Just sid) : sessions)
    (Tmux.View (Ident.Str "vim") (Just wid) : windows)
    (Tmux.View (Ident.Str "vim") (Just pid) : ps)
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
    TreeLeaf ((consPane (Ident.Str "make")) { _extra = Pane False True Nothing }),
    TreeLeaf ((consPane (Ident.Str "scratch")) { _extra = Pane False False Nothing, _geometry = scratchGeometry })
    ]

mainTree :: ViewGeometry -> ViewTree
mainTree vimGeometry =
  Tree (consLayout (Ident.Str "main")) [TreeNode (vimTree vimGeometry), TreeNode makeTree]

setupDefaultUi ::
  Members [Settings, AtomicState UiState] r =>
  Sem r ()
setupDefaultUi = do
  vimGeometry <- Settings.get Settings.vimPaneGeometry
  insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") (mainTree vimGeometry)]

setupDefaultTestUi ::
  Members [Settings, AtomicState UiState, AtomicState Views] r =>
  Sem r ()
setupDefaultTestUi = do
  setupDefaultUi
  atomicModify' (insertInitialViews (SessionId 0) (WindowId 0) (PaneId 0))

containsVimPid ::
  Member (Proc !! ProcError) r =>
  Pid ->
  Codec.PanePid ->
  Sem r Bool
containsVimPid target (Codec.PanePid _ candidate) =
  resumeAs False do
    elem (Pid candidate) <$> Proc.parentPids target

detectVimPidPane ::
  Members [TmuxPanes PanePid, TmuxPanes PaneCoords, Proc !! ProcError, Stop DetectUiError] r =>
  Pid ->
  Sem r Codec.PaneCoords
detectVimPidPane vpid = do
  shellPids <- panes
  Codec.PanePid paneId _ <- stopNote VimPaneNotFound =<< findM (containsVimPid vpid) shellPids
  stopNote (Unexpected "pane disappeared") =<< pane paneId

detectVimPane ::
  Members (NativeCodecsE [Panes PanePid, Panes PaneCoords]) r =>
  Members [NativeTmux !! TmuxError, Rpc, Proc !! ProcError, Stop DetectUiError] r =>
  Sem r Codec.PaneCoords
detectVimPane =
  mapUserMessage @ViewError Unexpected $ resumeHoist @TmuxError @NativeTmux ViewError.TmuxApi $ mapStop ViewError.TmuxCodec do
    withTmuxApisNative_ @[Panes PanePid, Panes PaneCoords] do
      result <- vimPid
      detectVimPidPane (Pid result)

-- TODO
detectDefaultUi ::
  Members [Proc !! ProcError, Rpc !! RpcError] r =>
  Members (NativeCodecsE [Panes PanePid, Panes PaneCoords]) r =>
  Members [NativeTmux !! TmuxError, Settings, AtomicState Views, AtomicState UiState, Stop DetectUiError] r =>
  Sem r ()
detectDefaultUi =
  resumeHoistUserMessage Unexpected do
    setupDefaultUi
    Codec.PaneCoords sid wid pid <- detectVimPane
    atomicModify' (insertInitialViews sid wid pid)
    atomicModify' @UiState (#vimPaneId ?~ pid)
