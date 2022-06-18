module Myo.Ui.Update where

-- import Chiasma.Data.Ident (generateIdent)
-- import Chiasma.Ui.Data.View (Layout(Layout), Pane(Pane), View(View))
-- import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
-- import Chiasma.Ui.Data.ViewState (ViewState(ViewState))
-- import Data.MessagePack (Object)
-- import Ribosome.Data.SettingError (SettingError)
-- import Ribosome.Msgpack.Decode (fromMsgpack')
-- import Ribosome.Msgpack.Error (DecodeError)

-- import Myo.Ui.Data.AddLayoutOptions (AddLayoutOptions(..))
-- import Myo.Ui.Data.AddPaneOptions (AddPaneOptions(..))
-- import Myo.Ui.Data.UiSettingCodec (UiSettingCodec(UiSettingCodec))
-- import Myo.Ui.Data.UiState (UiState)
-- import qualified Myo.Ui.Data.UiState as UiState (spaces)
-- import Myo.Ui.Default (setupDefaultUi)
-- import Myo.Ui.UpsertView (upsertLayoutSpaces, upsertPaneSpaces)

-- insertOrUpdateLayout ::
--   Member (AtomicState Env) r =>
--   Ident ->
--   View Layout ->
--   m ()
-- insertOrUpdateLayout ident layout =
--   modifyL @UiState UiState.spaces (upsertLayoutSpaces ident layout)

-- insertOrUpdatePane ::
--   Member (AtomicState Env) r =>
--   Ident ->
--   View Pane ->
--   m ()
-- insertOrUpdatePane ident pane =
--   modifyL @UiState UiState.spaces (upsertPaneSpaces ident pane)

-- createPane ::
--   MonadIO m =>
--   AddPaneOptions ->
--   m (Ident, View Pane)
-- createPane AddPaneOptions {..} = do
--   effectiveIdent <- maybe generateIdent pure ident
--   pure (layout, View effectiveIdent st geometry extra)
--   where
--     st = ViewState (fromMaybe False minimized)
--     geometry = ViewGeometry minSize maxSize fixedSize minimizedSize weight position
--     extra = Pane False (fromMaybe False pin) Nothing

-- createLayout ::
--   MonadIO m =>
--   AddLayoutOptions ->
--   m (Ident, View Layout)
-- createLayout (AddLayoutOptions layout vertical i minimized minSize maxSize fixedSize ms weight position) = do
--   ident <- maybe generateIdent pure i
--   pure (layout, View ident st geometry extra)
--   where
--     st = ViewState (fromMaybe False minimized)
--     geometry = ViewGeometry minSize maxSize fixedSize ms weight position
--     extra = Layout (fromMaybe False vertical)

-- resetUi ::
--   Member (AtomicState Env) r =>
--   m ()
-- resetUi = do
--   setL @UiState UiState.spaces []
--   setupDefaultUi

-- updateUi ::
--   MonadIO m =>
--   Member (AtomicState Env) r =>
--   Object ->
--   m ()
-- updateUi o = do
--   UiSettingCodec layouts panes <- fromMsgpack' o
--   resetUi
--   traverse_ (uncurry insertOrUpdateLayout) =<< traverse createLayout (fold layouts)
--   traverse_ (uncurry insertOrUpdatePane) =<< traverse createPane (fold panes)
