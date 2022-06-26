module Myo.Data.ViewError where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Chiasma.Effect.Codec (NativeCodec, NativeCodecE)
import Log (Severity (Error))
import Ribosome (ErrorMessage (ErrorMessage), HandlerError, ToErrorMessage (toErrorMessage), mapHandlerError)

data ViewError =
  TmuxApi TmuxError
  |
  TmuxCodec CodecError
  |
  TmuxViews ViewsError
  deriving stock (Eq, Show)

instance ToErrorMessage ViewError where
  toErrorMessage = \case
    TmuxApi e ->
      ErrorMessage "tmux api error" ["ViewError.TmuxApi:", show e] Error
    TmuxCodec e ->
      ErrorMessage "tmux codec error" ["ViewError.TmuxCodec:", show e] Error
    TmuxViews e ->
      ErrorMessage "tmux views error" ["ViewError.TmuxViews:", show e] Error

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
  Members [NativeCodecE cmd, Stop HandlerError] r =>
  InterpreterFor (NativeCodec cmd) r
handlerCodecError =
  mapHandlerError . resumeCodecError . raiseUnder
{-# inline handlerCodecError #-}

viewsError ::
  Member (Stop ViewError) r =>
  InterpreterFor (Stop ViewsError) r
viewsError =
  mapStop TmuxViews
{-# inline viewsError #-}

handlerViewsError ::
  Member (Stop HandlerError) r =>
  InterpreterFor (Stop ViewsError) r
handlerViewsError =
  mapHandlerError . mapStop TmuxViews . raiseUnder
{-# inline handlerViewsError #-}
