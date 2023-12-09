module Myo.Command.Run where

import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text
import Ribosome (
  Args (Args),
  Handler,
  Report,
  ReportLog,
  Rpc,
  RpcError,
  logReport,
  mapReport,
  resumeReport,
  resumeReports,
  )

import Myo.Command.Data.Command (Command (..), shellCommand, systemCommand)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSpec (parseCommandSpec')
import qualified Myo.Command.Data.HistoryEntry
import Myo.Command.Data.HistoryEntry (HistoryEntry)
import Myo.Command.Data.Param (ParamValues)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (RunLineOptions (RunLineOptions), line)
import Myo.Command.Data.UiTarget (UiTarget (UiTarget))
import qualified Myo.Command.Optparse as Optparse
import Myo.Command.Optparse (OptparseArgs)
import Myo.Command.Override (queryRegularOverrides)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Data.CommandQuery (queryAny, queryId, queryIdH, queryIndex)
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)
import qualified Myo.Effect.History as History
import Myo.Effect.History (History)

runAsync ::
  Members [Async, ReportLog] r =>
  Handler r () ->
  Sem r ()
runAsync action =
  void $ async $ runStop action >>= leftA \ err ->
    logReport err

runIdent ::
  Members [Controller !! RunError, Commands !! CommandError, Stop Report] r =>
  CommandId ->
  ParamValues ->
  Sem r ()
runIdent ident params =
  resumeReport @Controller $ resumeReport @Commands do
    cmd <- Commands.queryId ident
    Controller.runCommand cmd params Nothing

runIdentAsync ::
  Members [Controller !! RunError, Commands !! CommandError, Async, ReportLog] r =>
  CommandId ->
  ParamValues ->
  Sem r ()
runIdentAsync ident params =
  runAsync (runIdent ident params)

runCommand ::
  Members [Controller !! RunError, Stop Report] r =>
  Command ->
  ParamValues ->
  Maybe OptparseArgs ->
  Sem r ()
runCommand cmd params optparseArgs =
  resumeReport @Controller do
    Controller.runCommand cmd params optparseArgs

runCommandAsync ::
  Members [Controller !! RunError, Commands !! CommandError, Async, ReportLog] r =>
  Command ->
  ParamValues ->
  Sem r ()
runCommandAsync ident params =
  runAsync (runCommand ident params Nothing)

myoRun ::
  Members [Controller !! RunError, Commands !! CommandError, Rpc !! RpcError] r =>
  Text ->
  Args ->
  Handler r ()
myoRun ident args =
  resumeReports @[Commands, Rpc] @[_, _] do
    base <- Commands.query (queryAny (Text.strip ident))
    (cmd, params) <- mapReport (queryRegularOverrides base)
    runCommand cmd params (Optparse.fromArgs args)

lookupHistory ::
  Member History r =>
  Either CommandId Int ->
  Sem r HistoryEntry
lookupHistory =
  History.query . either queryIdH queryIndex

-- TODO support optparse
reRun ::
  Members [Controller !! RunError, History !! RunError, Stop Report] r =>
  Either CommandId Int ->
  Maybe ParamValues ->
  Sem r ()
reRun target params =
  resumeReport @Controller do
    entry <- resumeReport @History (lookupHistory target)
    Controller.runCommand entry.command (fold params <> foldMap (.params) entry.execution) Nothing

reRunAsync ::
  Members [Controller !! RunError, History !! RunError, Async, ReportLog] r =>
  Either CommandId Int ->
  Maybe ParamValues ->
  Sem r ()
reRunAsync target params =
  runAsync (reRun target params)

myoReRun ::
  Members [Controller !! RunError, History !! RunError] r =>
  Either CommandId Int ->
  Handler r ()
myoReRun spec =
  reRun spec mempty

defaultTarget :: UiTarget
defaultTarget = "make"

findTarget ::
  Member (Commands !! CommandError) r =>
  Ident ->
  Sem r (Either CommandId UiTarget)
findTarget target =
  (shell <$> Commands.query (queryId (CommandId target))) !> Right (UiTarget target)
  where
    shell cmd = Left cmd.ident

myoLine ::
  Members [Controller !! RunError, Commands !! CommandError, Input Ident] r =>
  RunLineOptions ->
  Handler r ()
myoLine (RunLineOptions mayLine mayLines mayTarget lang skipHistory kill capture) =
  resumeReport @Controller $ mapReport @RunError do
    ident <- CommandId <$> input
    lines' <- stopNote RunError.NoLinesSpecified (mayLines <|> mayLine)
    target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
    Controller.runCommand (cmd ident target lines') mempty Nothing
  where
    cmd ident target cmdLines =
      (cons target ident cmdLines) {
        lang,
        skipHistory = orFalse skipHistory,
        kill = orFalse kill,
        capture = orFalse capture
      }
    cons =
      either shellCommand (systemCommand . Just)

myoLineCmd ::
  Members [Controller !! RunError, Commands !! CommandError, Input Ident] r =>
  Args ->
  Handler r ()
myoLineCmd (Args cmdLine) =
  myoLine def { line = Just (parseCommandSpec' [cmdLine]) }
