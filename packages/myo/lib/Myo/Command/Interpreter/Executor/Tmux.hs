module Myo.Command.Interpreter.Executor.Tmux where

import Chiasma.Codec.Data (Pane)
import Chiasma.Command.Pane (sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.TmuxId (PaneId)
import Chiasma.Data.TmuxRequest (TmuxRequest)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Chiasma.Tmux (withTmuxApis)
import qualified Chiasma.View as Views
import Exon (exon)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command), ident)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (RunTask), RunTaskDetails (UiSystem))
import Myo.Command.Data.TmuxTask (TmuxTask (TmuxTask))
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import Myo.Ui.Data.UiState (UiState)

acceptCommand ::
  PaneId ->
  Command ->
  Maybe TmuxTask
acceptCommand pid = \case
  cmd ->
    Just (TmuxTask pid cmd)

cmdline :: [Text] -> [Key]
cmdline =
  fmap Lit

paneIdForIdent ::
  Members [AtomicState Views, Stop ViewsError] r =>
  Ident ->
  Sem r PaneId
paneIdForIdent paneIdent =
  stopEither =<< atomicGets (Views.paneId paneIdent)

runInTmux ::
  -- ∀ eres r .
  -- Member (Events eres RunEvent) r =>
  Member (Codec TmuxCommand (Const TmuxRequest) (Const [Text]) !! CodecError) r =>
  Member (Codec (Panes Pane) (Const TmuxRequest) (Const [Text]) !! CodecError) r =>
  Members [ScopedTmux () (Const TmuxRequest) (Const [Text]), AtomicState UiState, Embed IO] r =>
  TmuxTask ->
  Sem r (Maybe [Text])
runInTmux (TmuxTask paneId Command {cmdLines}) =
  withTmuxApis @[TmuxCommand, Panes _] $ resuming tmuxError do
    sendKeys paneId (cmdline cmdLines)
    pure Nothing
  where
    tmuxError e =
      pure (Just [show e])

interpretExecutorTmux ::
  -- Member (Events eres RunEvent) r =>
  Member (Codec TmuxCommand (Const TmuxRequest) (Const [Text]) !! CodecError) r =>
  Member (Codec (Panes Pane) (Const TmuxRequest) (Const [Text]) !! CodecError) r =>
  Members [ScopedTmux () (Const TmuxRequest) (Const [Text]) !! TmuxError, AtomicState UiState, Embed IO] r =>
  Member (AtomicState Views) r =>
  InterpreterFor (Executor TmuxTask !! RunError) r
interpretExecutorTmux =
  interpretResumable \case
    Executor.Accept (RunTask cmd _ (UiSystem target)) -> do
      paneId <- mapStop RunError.Views (paneIdForIdent target)
      pure (acceptCommand paneId cmd)
    Executor.Accept _ ->
      pure Nothing
    Executor.Run task@(TmuxTask _ Command {ident}) ->
      runInTmux task !! \ (e :: TmuxError) -> pure (Just [[exon|tmux execution failed for #{identText ident}|], show e])
