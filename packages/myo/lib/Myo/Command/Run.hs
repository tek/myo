module Myo.Command.Run where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident, generateIdent)
import qualified Data.Text as Text
import Ribosome (Handler, HostError, mapHandlerError, resumeHandlerError)

import Myo.Command.Command (commandByIdentOrName, mayCommandByIdent, shellCommand, systemCommand)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (RunLineOptions (RunLineOptions))
import Myo.Command.History (lookupHistory)
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
  resumeHandlerError @Controller $ mapHandlerError do
    Controller.runCommand =<< commandByIdentOrName "run" (Text.strip ident)

reRun ::
  Members [Controller, AtomicState CommandState, Stop CommandError] r =>
  Either Ident Int ->
  Sem r ()
reRun =
  Controller.runCommand <=< lookupHistory

myoReRun ::
  Members [Controller !! RunError, AtomicState CommandState] r =>
  Either Ident Int ->
  Handler r ()
myoReRun spec =
  resumeHandlerError @Controller $ mapHandlerError do
    reRun spec

defaultTarget :: Ident
defaultTarget =
  Ident.Str "make"

myoLine ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO, DataLog HostError] r =>
  RunLineOptions ->
  Handler r ()
myoLine (RunLineOptions mayLine mayLines mayTarget runner lang skipHistory kill capture) =
  resumeHandlerError @Controller $ mapHandlerError do
    ident <- generateIdent
    lines' <- stopNote RunError.NoLinesSpecified (mayLines <|> (pure <$> mayLine))
    target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
    Controller.runCommand (cmd ident target lines')
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
