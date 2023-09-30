module Myo.Command.Run where

import Chiasma.Data.Ident (generateIdent)
import qualified Data.Text as Text
import Exon (exon)
import qualified Log
import Ribosome (Args (Args), Handler, Report, mapReport, resumeReport)

import Myo.Command.Command (commandByIdent, commandByIdentOrName, mayCommandByIdent, shellCommand, systemCommand)
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandSpec (parseCommandSpec')
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (RunLineOptions (RunLineOptions), line)
import Myo.Command.Data.UiTarget (UiTarget (UiTarget))
import Myo.Command.History (lookupHistory)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)

runAsync ::
  Members [Async, Log] r =>
  Handler r () ->
  Sem r ()
runAsync action =
  void $ async $ runStop action >>= leftA \ err ->
    Log.debug [exon|Running async command failed: #{show err}|]

runIdent ::
  Members [Controller !! RunError, AtomicState CommandState, Stop Report] r =>
  CommandId ->
  ParamValues ->
  Sem r ()
runIdent ident params =
  resumeReport @Controller $ mapReport do
    cmd <- commandByIdent "run" ident
    Controller.runCommand cmd params

runIdentAsync ::
  Members [Controller !! RunError, AtomicState CommandState, Async, Log] r =>
  CommandId ->
  ParamValues ->
  Sem r ()
runIdentAsync ident params =
  runAsync (runIdent ident params)

runCommand ::
  Members [Controller !! RunError, AtomicState CommandState, Stop Report] r =>
  Command ->
  ParamValues ->
  Sem r ()
runCommand cmd params =
  resumeReport @Controller do
    Controller.runCommand cmd params

runCommandAsync ::
  Members [Controller !! RunError, AtomicState CommandState, Async, Log] r =>
  Command ->
  ParamValues ->
  Sem r ()
runCommandAsync ident params =
  runAsync (runCommand ident params)

myoRun ::
  Members [Controller !! RunError, AtomicState CommandState] r =>
  Text ->
  Handler r ()
myoRun ident = do
  cmd <- mapReport $ commandByIdentOrName "run" (Text.strip ident)
  runCommand cmd mempty

reRun ::
  Members [Controller !! RunError, AtomicState CommandState, Stop Report] r =>
  Either CommandId Int ->
  Maybe ParamValues ->
  Sem r ()
reRun target params =
  resumeReport @Controller $ mapReport do
    entry <- lookupHistory target
    Controller.runCommand entry.command (maybe (fold params) (.params) entry.execution)

reRunAsync ::
  Members [Controller !! RunError, AtomicState CommandState, Async, Log] r =>
  Either CommandId Int ->
  Maybe ParamValues ->
  Sem r ()
reRunAsync target params =
  runAsync (reRun target params)

myoReRun ::
  Members [Controller !! RunError, AtomicState CommandState] r =>
  Either CommandId Int ->
  Handler r ()
myoReRun spec =
  resumeReport @Controller $ mapReport do
    reRun spec mempty

defaultTarget :: UiTarget
defaultTarget = "make"

myoLine ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO] r =>
  RunLineOptions ->
  Handler r ()
myoLine (RunLineOptions mayLine mayLines mayTarget runner lang skipHistory kill capture) =
  resumeReport @Controller $ mapReport do
    ident <- CommandId <$> generateIdent
    lines' <- stopNote RunError.NoLinesSpecified (mayLines <|> mayLine)
    target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
    Controller.runCommand (cmd ident target lines') mempty
  where
    cmd ident target cmdLines =
      (cons target ident cmdLines) {
        runner,
        lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }
    cons =
      either shellCommand (systemCommand . Just)
    findTarget target =
      maybe (Right (UiTarget target)) (Left . (.ident)) <$> mayCommandByIdent (CommandId target)

myoLineCmd ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO] r =>
  Args ->
  Handler r ()
myoLineCmd (Args cmdLine) =
  myoLine def { line = Just (parseCommandSpec' [cmdLine]) }
