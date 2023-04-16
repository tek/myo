module Myo.Test.RunTest where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Map.Strict as Map
import Log (Severity (Error))
import Polysemy.Test (Hedgehog, UnitTest, assertJust, evalLeft)
import Ribosome (
  LogReport,
  Report (Report),
  ReportContext,
  Reportable (toReport),
  Reports,
  StoredReport (StoredReport),
  interpretPersistNull,
  logReport,
  storedReports,
  )
import qualified Ribosome.Report as Report
import Ribosome.Test (testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.Command (Command (Command, cmdLines))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (line, runner)
import Myo.Command.Data.RunTask (RunTask (RunTask), command)
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend, interpretBackendFail)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Run (myoLine, myoRunIdent)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Controller as Controller
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoTest)

newtype RunTestError =
  RunTestError { unTestError :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

instance Reportable RunTestError where
  toReport (RunTestError e) =
    Report (unlines e) e Error

testError :: Text
testError =
  "error"

ctx :: ReportContext
ctx = "test"

runnerIdent :: Ident
runnerIdent =
  Str "dummy"

ident :: CommandId
ident =
  "cmd"

checkReport ::
  Members [Hedgehog IO, Reports] r =>
  [Text] ->
  Sem r ()
checkReport target = do
  loggedError <- Map.lookup ctx <$> storedReports
  assertJust [target] (fmap user <$> loggedError)
  where
    user (StoredReport (Report _ l _) _) =
      l

dummyAccept :: RunTask -> Sem r (Maybe ())
dummyAccept _ =
  pure (Just ())

dummyExecute ::
  Member (DataLog LogReport) r =>
  () ->
  Sem r ()
dummyExecute () =
  Report.setContext ctx do
    logReport (RunTestError [testError])

interpretBackendDummy ::
  Member (DataLog LogReport) r =>
  InterpreterFor (Backend !! RunError) r
interpretBackendDummy =
  interpretBackendFail .
  interceptBackend dummyAccept dummyExecute (captureUnsupported "dummy") unit

test_runSystem :: UnitTest
test_runSystem =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendDummy $ interpretController do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      myoRunIdent ident
    checkReport [testError]

cmdline :: Text
cmdline = "echo 'hello'"

singleLineAccept :: RunTask -> Sem r (Maybe [Text])
singleLineAccept RunTask {command = Command {cmdLines = l}} =
  pure (Just l)

singleLineExecute ::
  Member (DataLog LogReport) r =>
  [Text] ->
  Sem r ()
singleLineExecute l =
  Report.setContext ctx do
    logReport (RunTestError l)

interpretBackendDummySingleLine ::
  Member (DataLog LogReport) r =>
  InterpreterFor (Backend !! RunError) r
interpretBackendDummySingleLine =
  interpretBackendFail .
  interceptBackend singleLineAccept singleLineExecute (captureUnsupported "dummy") unit

test_runLineSingle :: UnitTest
test_runLineSingle =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendDummySingleLine $ interpretController do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      myoLine def { line = Just cmdline, runner = Just runnerIdent }
    checkReport [cmdline]

test_runSubprocFail :: UnitTest
test_runSubprocFail =
  myoTest $ interpretCommandLogSetting $ interpretPersistNull $ interpretBackendProcessNative $ interpretController do
    r <- testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls -234234"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      resumeEither (Controller.runIdent ident)
    _ <- evalLeft r
    unit
