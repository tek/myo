module Myo.Tmux.Quit where

import Chiasma.Command.Pane (closePane)
import Chiasma.Data.TmuxError (TmuxError)
import qualified Chiasma.Data.View
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.TmuxNative (withTmuxNative_)

import qualified Myo.Data.ViewError as ViewError
import Myo.Data.ViewError (ViewError)
import Myo.Ui.Data.UiState (UiState)

-- |Close all panes except for the one containing vim.
-- If no vim pane id was stored, no action will be performed.
closePanes ::
  Members [NativeTmux !! TmuxError, NativeCommandCodecE, AtomicState UiState, AtomicState Views, Stop ViewError] r =>
  Sem r ()
closePanes =
  mapStop ViewError.TmuxApi $ mapStop ViewError.TmuxCodec do
    traverse_ close =<< atomicView @UiState #vimPaneId
  where
    close vimPaneId = do
      panes <- atomicView @Views #_panes
      restop @TmuxError @NativeTmux $ withTmuxNative_ do
        traverse_ closePane (filter (vimPaneId /=) (mapMaybe (.id) panes))
