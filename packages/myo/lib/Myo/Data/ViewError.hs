module Myo.Data.ViewError where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError (NoExe))
import Chiasma.Data.Views (ViewsError)
import Chiasma.Effect.Codec (NativeCodec, NativeCodecE)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Log (Severity (Error))
import Ribosome (Report (Report), Reportable (toReport), mapReport)

data ViewError =
  TmuxApi TmuxError
  |
  TmuxCodec CodecError
  |
  TmuxViews ViewsError
  |
  TreeMod TreeModError
  |
  Render RenderError
  deriving stock (Eq, Show)

instance Reportable ViewError where
  toReport = \case
    TmuxApi NoExe ->
      Report "Tmux isn't available" ["ViewError.TmuxApi:", show NoExe] Error
    TmuxApi e ->
      Report "tmux api error" ["ViewError.TmuxApi:", show e] Error
    TmuxCodec e ->
      Report "tmux codec error" ["ViewError.TmuxCodec:", show e] Error
    TmuxViews e ->
      Report "tmux views error" ["ViewError.TmuxViews:", show e] Error
    TreeMod e ->
      Report "tmux views error" ["ViewError.TreeMod:", show e] Error
    Render e ->
      Report "tmux views error" ["ViewError.Render:", show e] Error

tmuxError ::
  Member (Stop ViewError) r =>
  InterpreterFor (Stop TmuxError) r
tmuxError =
  mapStop TmuxApi
{-# inline tmuxError #-}

resumeTmuxError ::
  ∀ eff r .
  Members [eff !! TmuxError, Stop ViewError] r =>
  InterpreterFor eff r
resumeTmuxError =
  resumeHoist TmuxApi
{-# inline resumeTmuxError #-}

codecError ::
  Member (Stop ViewError) r =>
  InterpreterFor (Stop CodecError) r
codecError =
  mapStop TmuxCodec
{-# inline codecError #-}

resumeCodecError ::
  ∀ eff r .
  Members [eff !! CodecError, Stop ViewError] r =>
  InterpreterFor eff r
resumeCodecError =
  resumeHoist TmuxCodec
{-# inline resumeCodecError #-}

handlerCodecError ::
  ∀ cmd r .
  Members [NativeCodecE cmd, Stop Report] r =>
  InterpreterFor (NativeCodec cmd) r
handlerCodecError =
  mapReport . resumeCodecError . raiseUnder
{-# inline handlerCodecError #-}

viewsError ::
  Member (Stop ViewError) r =>
  InterpreterFor (Stop ViewsError) r
viewsError =
  mapStop TmuxViews
{-# inline viewsError #-}

handlerViewsError ::
  Member (Stop Report) r =>
  InterpreterFor (Stop ViewsError) r
handlerViewsError =
  mapReport . mapStop TmuxViews . raiseUnder
{-# inline handlerViewsError #-}
