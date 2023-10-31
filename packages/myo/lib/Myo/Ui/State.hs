module Myo.Ui.State where

import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Views (ViewsError (NoSuchPane))
import Chiasma.Lens.Tree (leavesIdent)
import qualified Chiasma.Ui.Data.View
import Chiasma.Ui.Data.View (LayoutView, PaneView, ViewTree, leaves, treesIdent)
import Control.Lens (anyOf, firstOf)
import Control.Lens.Extras (template)
import Ribosome (Handler, MsgpackDecode, MsgpackEncode, mapReport)

import Myo.Data.ViewError (ViewError (NoSuchLayout, TmuxViews))
import Myo.Orphans ()
import Myo.Ui.Data.UiState (UiState)

data LayoutState =
  LayoutState {
    open :: Bool,
    view :: LayoutView
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackEncode, MsgpackDecode)

myoLayoutState ::
  Member (AtomicState UiState) r =>
  Ident ->
  Handler r LayoutState
myoLayoutState ident =
  mapReport do
    layout <- stopNote (NoSuchLayout ident) =<< atomicGets (firstOf (template @_ @ViewTree . treesIdent ident))
    pure LayoutState {
      open = anyOf (leaves . #extra . #open) id layout,
      view = layout.treeData
    }

myoPaneState ::
  Member (AtomicState UiState) r =>
  Ident ->
  Handler r PaneView
myoPaneState ident =
  mapReport do
    stopNote (TmuxViews (NoSuchPane ident)) =<< atomicGets (firstOf (template @_ @ViewTree . leavesIdent ident))
