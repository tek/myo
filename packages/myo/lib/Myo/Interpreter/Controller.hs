module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (identText)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.Views (Views)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Exon (exon)
import Log (Severity (Error))
import Ribosome (ErrorMessage (ErrorMessage), HostError, Rpc, reportError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask as RunTaskDetails
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Data.TmuxTask (TmuxTask)
import Myo.Command.RunTask (runTask)
import Myo.Data.ProcessTask (ProcessTask)
import Myo.Effect.Controller (Controller (RunCommand))
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Toggle (ToggleStack, ensurePaneOpen)

handleError ::
  Member (DataLog HostError) r =>
  Command ->
  [Text] ->
  Sem r ()
handleError Command {ident} errors =
  reportError (Just "command") (message errors Error)
  where
    message = \case
      e ->
        ErrorMessage [exon|#{identText ident} failed#{foldMap userMessage (head e)}|]
        (["Controller: command failed", identText ident] <> e)
    userMessage m =
      [exon|: #{m}|]

prepare ::
  ∀ enc dec r .
  Members (ToggleStack enc dec) r =>
  Members [Rpc, AtomicState UiState, AtomicState Views, Stop RenderError, Stop ToggleError] r =>
  RunTask ->
  Sem r ()
prepare = \case
  RunTask _ _ (RunTaskDetails.UiSystem ident) ->
    ensurePaneOpen ident
  RunTask _ _ _ ->
    undefined

tryExecutor ::
  ∀ task enc dec r .
  Members (ToggleStack enc dec) r =>
  Members [Rpc, Executor task !! RunError, AtomicState UiState, AtomicState Views, Stop RunError, Stop RenderError] r =>
  RunTask ->
  Sem r (Maybe (Maybe [Text]))
tryExecutor rtask =
  restop @_ @(Executor _) do
    traverse Executor.run =<< Executor.accept rtask

runCommand ::
  Members (ToggleStack enc dec) r =>
  Members [Executor ProcessTask !! RunError, Executor TmuxTask !! RunError] r =>
  Members [Rpc, AtomicState UiState, AtomicState Views, Stop RunError, Stop RenderError] r =>
  Members [Reader LogDir, AtomicState CommandState, DataLog HostError, Stop ToggleError] r =>
  Command ->
  Sem r ()
runCommand cmd = do
  task <- mapStop RunError.Command (runTask cmd)
  prepare task
  maybe (stop (RunError.NoRunner task)) (traverse_ (handleError cmd)) =<< runMaybeT do
    MaybeT (tryExecutor @TmuxTask task) <|> MaybeT (tryExecutor @ProcessTask task)

interpretController ::
  Members (ToggleStack enc dec) r =>
  Members [Executor ProcessTask !! RunError, Executor TmuxTask !! RunError] r =>
  Members [Rpc, AtomicState UiState, AtomicState Views] r =>
  Members [Reader LogDir, AtomicState CommandState, DataLog HostError] r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunCommand cmd ->
      mapStop RunError.Toggle $ mapStop RunError.Render $ mapStop RunError.TreeMod do
        runCommand cmd
