module Myo.Command.Run where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident, generateIdent)
import qualified Data.Text as Text
import Ribosome (Handler, HostError, mapHandlerError, reportError, resumeHandlerError)

import Myo.Command.Command (commandByIdentOrName, mayCommandByIdent, shellCommand, systemCommand)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (RunLineOptions (RunLineOptions))
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)
import Myo.Orphans ()

-- preRunSystem ::
--   Member (AtomicState Env) r =>
--   RunTask ->
--   Runner ->
--   m ()
-- preRunSystem task@(RunTask (Command _ cmdIdent _ _ _ _ _ _ _) log _) runner = do
--   checkPending <- hoistEither =<< liftIO (runnerCheckPending runner task)
--   closeOutputSocket cmdIdent
--   pushExecution cmdIdent checkPending
--   monitorCommand cmdIdent log

-- preRun ::
--   MyoRender s e m =>
--   Member (AtomicState Env) r =>
--   RunTask ->
--   Runner ->
--   m ()
-- preRun task@(RunTask _ _ (RunTaskDetails.UiSystem ident)) runner = do
--   ensurePaneOpen ident
--   preRunSystem task runner
-- preRun (RunTask _ _ (RunTaskDetails.UiShell shellIdent _)) _ = do
--   active <- isCommandActive shellIdent
--   unless active do
--     Log.debug $ "starting inactive shell command `" <> identText shellIdent <> "`"
--     myoRun (identText shellIdent)
-- preRun task@(RunTask _ _ RunTaskDetails.System) runner = do
--   preRunSystem task runner
-- preRun _ _ =
--   pure ()

-- postRun ::
--   Member (AtomicState Env) r =>
--   RunTask ->
--   m ()
-- postRun (RunTask cmd _ (RunTaskDetails.UiSystem _)) =
--   pushHistory cmd
-- postRun (RunTask cmd _ (RunTaskDetails.UiShell _ _)) =
--   pushHistory cmd
-- postRun _ =
--   pure ()

-- executeRunner ::
--   Runner ->
--   RunTask ->
--   m ()
-- executeRunner (Runner _ _ run _ _) task = do
--   Log.debug $ "executing runner for command `" <> identText ident <> "`"
--   hoistEither =<< liftIO (run task)
--   where
--     ident = Lens.view Command.ident . RunTask.command $ task

-- |Main entry point for running commands that ensures consistency.
-- Saves all buffers, updating the 'lastSave' timestamp.
-- Selects the proper runner for the task, e.g. tmux.
-- Sets up the output watcher threads that connect to a socket; the implementation of the runner is expected to ensure
-- that output is redirected to this socket.
-- Pushes the command into the history.
runCommand ::
  Members [Controller !! RunError, AtomicState CommandState, DataLog HostError] r =>
  Command ->
  Sem r ()
runCommand cmd = do
  -- preCommandSave
  -- pushCommandLog (Lens.view Command.ident cmd)
  _ <- Controller.runCommand cmd !! \ e -> reportError (Just "command") e
  unit
  -- task <- runTask cmd
  -- runner <- findRunner task
  -- preRun task runner
  -- executeRunner runner task
  -- void $ postRun task

myoRunIdent ::
  Members [Controller !! RunError, AtomicState CommandState, DataLog HostError] r =>
  Ident ->
  Handler r ()
myoRunIdent i =
  resumeHandlerError do
    Controller.runIdent i

myoRun ::
  Members [Controller !! RunError, AtomicState CommandState, DataLog HostError] r =>
  Text ->
  Handler r ()
myoRun ident =
  mapHandlerError do
    runCommand =<< commandByIdentOrName "run" (Text.strip ident)

-- myoReRun ::
--   MyoRender s e m =>
--   Member (AtomicState Env) r =>
--   Either Ident Int ->
--   m ()
-- myoReRun =
--   runCommand <=< lookupHistory

defaultTarget :: Ident
defaultTarget =
  Ident.Str "make"

myoLine ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO, DataLog HostError] r =>
  RunLineOptions ->
  Handler r ()
myoLine (RunLineOptions mayLine mayLines mayTarget runner lang skipHistory kill capture) =
  mapHandlerError do
    ident <- generateIdent
    lines' <- stopNote RunError.NoLinesSpecified (mayLines <|> (pure <$> mayLine))
    target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
    runCommand (cmd ident target lines')
  where
    cmd ident target cmdLines =
      (cons target ident cmdLines) { runner, lang, skipHistory = orFalse skipHistory, kill = orFalse kill, capture = orFalse capture }
    cons =
      either shellCommand (systemCommand . Just)
    findTarget target =
      maybe (Right target) (Left . Command.ident) <$> mayCommandByIdent target

-- myoLineCmd ::
--   MyoRender s e m =>
--   Member (AtomicState Env) r =>
--   Text ->
--   m ()
-- myoLineCmd line' =
--   myoLine (RunLineOptions (Just line') Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
