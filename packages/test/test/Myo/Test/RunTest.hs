module Myo.Test.RunTest where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Map.Strict as Map
import Log (Severity (Error, Warn))
import Polysemy.Test (Hedgehog, UnitTest, assertJust, assertLeft)
import Ribosome (
  LogReport,
  Report (Report),
  ReportContext,
  Reportable (toReport),
  Reports,
  StoredReport (StoredReport),
  logReport,
  storedReports,
  )
import Ribosome.Api (defineFunction, nvimSetVar)
import qualified Ribosome.Report as Report
import Ribosome.Test (testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions, systemOptions)
import Myo.Command.Data.CommandSpec (parseCommandSpec')
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamId, ParamValue (ParamFlag))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (line, runner)
import qualified Myo.Command.Data.RunTask
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)
import Myo.Command.Interpreter.Backend.Process (interpretBackendProcessNative)
import Myo.Command.Run (myoLine, runIdent)
import Myo.Data.CommandId (CommandId)
import Myo.Interpreter.Controller (interpretControllerTransient)
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
  Members [Backend !! RunError, DataLog LogReport] r =>
  Sem r a ->
  Sem r a
interpretBackendDummy =
  interceptBackend dummyAccept dummyExecute (captureUnsupported "dummy") unit

test_runSystem :: UnitTest
test_runSystem =
  myoTest $ interpretBackendDummy $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      runIdent ident mempty
    checkReport [testError]

cmdline :: Text
cmdline = "echo 'hello'"

singleLineAccept :: RunTask -> Sem r (Maybe [Text])
singleLineAccept RunTask {compiled = l} =
  pure (Just l)

singleLineExecute ::
  Member (DataLog LogReport) r =>
  [Text] ->
  Sem r ()
singleLineExecute l =
  Report.setContext ctx do
    logReport (RunTestError l)

interpretBackendDummySingleLine ::
  Members [Backend !! RunError, DataLog LogReport] r =>
  Sem r a ->
  Sem r a
interpretBackendDummySingleLine =
  interceptBackend singleLineAccept singleLineExecute (captureUnsupported "dummy") unit

test_runLineSingle :: UnitTest
test_runLineSingle =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      myoLine def { line = Just (parseCommandSpec' [cmdline]), runner = Just runnerIdent }
    checkReport [cmdline]

test_runSubprocFail :: UnitTest
test_runSubprocFail =
  myoTest $ interpretBackendProcessNative $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand (AddSystemCommandOptions.cons ident ["ls -234234"]) { AddSystemCommandOptions.runner = Just runnerIdent }
      _ <- assertLeft err =<< runStop @Report (runIdent ident mempty)
      unit
  where
    err = Report "subprocess failed: exit code 2" ["RunError.SubprocFailed"] Warn

paramDefaults :: Map ParamId ParamDefault
paramDefaults =
  [("par2", "default value 2"), ("par3", "default value 3"), ("par5", ParamDefault (ParamFlag False))]

paramCommand :: AddSystemCommandOptions
paramCommand =
  systemOptions ident [line]
  & #runner ?~ runnerIdent
  & #params ?~ paramDefaults
  where
    line = "cmd: {par1} / {par2:sub ({par1}) ({par2})} / {par3} / {par4:}{par5?bool value 1}{par6?bool value 2}"

test_runParamCommand :: UnitTest
test_runParamCommand =
  myoTest $ interpretBackendDummySingleLine $ interpretControllerTransient [] do
    testHandler do
      myoAddSystemCommand paramCommand
      nvimSetVar @Text "myo_param_par1" "var value 1"
      nvimSetVar "myo_param_par6" True
      defineFunction "Myo_param_par2" [] ["return 'fun value 2'"]
      runIdent ident mempty
    checkReport ["cmd: var value 1 / sub (var value 1) (fun value 2) / default value 3 / bool value 2"]
