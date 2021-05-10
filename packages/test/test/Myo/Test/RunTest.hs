module Myo.Test.RunTest where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (at, view)
import Hedgehog (evalMaybe, (===))
import Ribosome.Control.Monad.Ribo (inspectErrors)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.Test.Run (UnitTest)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Unknown))
import Myo.Command.Data.RunLineOptions (RunLineOptions(RunLineOptions))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Command.Run (myoLine, myoRunIdent)
import Myo.Command.Runner (addRunner, extractRunError)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Data.Env (Myo)
import Myo.Test.Unit (MyoTest, intTestDef, testDef)

testError :: Text
testError = "error"

cname :: Text
cname = "test"

runnerIdent :: Ident
runnerIdent =
  Str "dummy"

runDummy :: RunTask -> Myo ()
runDummy _ =
  reportError cname ([testError] :: [Text])

addDummyRunner ::
  (RunTask -> Myo ()) ->
  Myo ()
addDummyRunner runner =
  addRunner runnerIdent (extractRunError runner) (extractRunError checkDummy) (const True) Nothing
  where
    checkDummy _ =
      return $ return ExecutionState.Unknown

check :: [Text] -> MyoTest ()
check target = do
  loggedError <- inspectErrors $ Lens.view $ Errors.componentErrors . Lens.at (ComponentName cname)
  let errorReport = fmap (Lens.view $ Errors.report . ErrorReport.user) <$> loggedError
  Just target === errorReport

runSystemTest :: MyoTest ()
runSystemTest = do
  let ident = Str "cmd"
  myoAddSystemCommand $ AddSystemCommandOptions ident ["ls"] (Just runnerIdent) Nothing Nothing Nothing Nothing Nothing Nothing
  lift (addDummyRunner runDummy)
  lift (myoRunIdent ident)
  check [testError]

test_runSystem :: UnitTest
test_runSystem =
  testDef runSystemTest

cmdline :: Text
cmdline = "echo 'hello"

lineRunner :: RunTask -> Myo ()
lineRunner (RunTask (Command _ _ lines' _ _ _ _ _ _) _ _) =
  reportError cname lines'

runLineSingleTest :: MyoTest ()
runLineSingleTest = do
  lift (addDummyRunner lineRunner)
  lift (myoLine (RunLineOptions (Just cmdline) Nothing Nothing (Just runnerIdent) Nothing Nothing Nothing Nothing))
  check [cmdline]

test_runLineSingle :: UnitTest
test_runLineSingle =
  testDef runLineSingleTest

runLineCmdTest :: MyoTest ()
runLineCmdTest =
  vimCommand "MyoLine echo 'hello'"

test_runLineCmd :: UnitTest
test_runLineCmd =
  intTestDef runLineCmdTest

runSubprocTest :: MyoTest ()
runSubprocTest = do
  let ident = Str "cmd"
  lift addSubprocessRunner
  myoAddSystemCommand $ AddSystemCommandOptions ident ["ls -234234"] (Just runnerIdent) Nothing Nothing Nothing Nothing Nothing Nothing
  lift (myoRunIdent ident)
  sleep 1
  lift (myoRunIdent ident)
  sleep 1
  loggedError <- evalMaybe =<< lift (inspectErrors (Lens.view $ Errors.componentErrors . Lens.at (ComponentName "subproc")))
  2 === length loggedError

test_runSubproc :: UnitTest
test_runSubproc =
  testDef runSubprocTest
