module Myo.Ui.Update where

import Chiasma.Data.Axis (Axis (Horizontal, Vertical))
import Chiasma.Data.Ident (Ident, generateIdent)
import Chiasma.Ui.Data.View (Layout (Layout), Pane (Pane), View (View))
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry))
import Chiasma.Ui.Data.ViewState (ViewState (ViewState))
import Data.MessagePack (Object)
import Ribosome (Handler, Settings, fromMsgpack, resumeReport, toReport)
import Ribosome.Data.SettingError (SettingError)

import Myo.Ui.Data.AddLayoutOptions (AddLayoutOptions (..))
import Myo.Ui.Data.AddPaneOptions (AddPaneOptions (..))
import Myo.Ui.Data.UiSettingCodec (UiSettingCodec (UiSettingCodec))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (setupDefaultUi)
import Myo.Ui.UpsertView (upsertLayoutSpaces, upsertPaneSpaces)

verticalAxis :: Bool -> Axis
verticalAxis = \case
  True -> Vertical
  False -> Horizontal

insertOrUpdateLayout ::
  Member (AtomicState UiState) r =>
  Ident ->
  View Layout ->
  Sem r ()
insertOrUpdateLayout ident layout =
  atomicModify'(#spaces %~ upsertLayoutSpaces ident layout)

insertOrUpdatePane ::
  Member (AtomicState UiState) r =>
  Ident ->
  View Pane ->
  Sem r ()
insertOrUpdatePane ident pane =
  atomicModify'(#spaces %~ upsertPaneSpaces ident pane)

createPane ::
  Member (Embed IO) r =>
  AddPaneOptions ->
  Sem r (Ident, View Pane)
createPane AddPaneOptions {..} = do
  effectiveIdent <- maybe generateIdent pure ident
  pure (layout, View effectiveIdent st geometry extra)
  where
    st = ViewState (fromMaybe False minimized)
    geometry = ViewGeometry minSize maxSize fixedSize minimizedSize weight position
    extra = Pane False (fromMaybe False pin) Nothing

createLayout ::
  Member (Embed IO) r =>
  AddLayoutOptions ->
  Sem r (Ident, View Layout)
createLayout (AddLayoutOptions layout vertical i minimized minSize maxSize fixedSize ms weight position) = do
  ident <- maybe generateIdent pure i
  pure (layout, View ident st geometry extra)
  where
    st = ViewState (fromMaybe False minimized)
    geometry = ViewGeometry minSize maxSize fixedSize ms weight position
    extra = Layout (maybe Horizontal verticalAxis vertical)

resetUi ::
  Members [AtomicState UiState, Settings] r =>
  Sem r ()
resetUi = do
  atomicSet #spaces []
  setupDefaultUi

updateUi ::
  Members [Settings !! SettingError, AtomicState UiState, Embed IO] r =>
  Object ->
  Handler r ()
updateUi o =
  resumeReport do
    UiSettingCodec layouts panes <- stopEitherWith toReport (fromMsgpack o)
    resetUi
    traverse_ (uncurry insertOrUpdateLayout) =<< traverse createLayout (fold layouts)
    traverse_ (uncurry insertOrUpdatePane) =<< traverse createPane (fold panes)
