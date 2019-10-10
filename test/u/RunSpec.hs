{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (at, view)
import Ribosome.Control.Monad.Ribo (inspectErrors)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Ribosome.Nvim.Api.IO (vimCommand)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState(Unknown))
import Myo.Command.Data.RunLineOptions (RunLineOptions(RunLineOptions))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Command.Run (myoLine, myoRun)
import Myo.Command.Runner (addRunner, extractRunError)
import Myo.Data.Env (Myo)
import Unit (intSpecDef, specDef)

testError :: Text
testError = "error"

cname :: Text
cname = "test"

runnerIdent :: Ident
runnerIdent =
  Str "dummy"

runDummy :: RunTask -> Myo ()
runDummy _ =
  reportError cname [testError]

addDummyRunner ::
  (RunTask -> Myo ()) ->
  Myo ()
addDummyRunner runner =
  addRunner runnerIdent (extractRunError runner) (extractRunError checkDummy) (const True)
  where
    checkDummy _ =
      return $ return ExecutionState.Unknown

check :: [Text] -> Myo ()
check target = do
  loggedError <- inspectErrors $ Lens.view $ Errors.componentErrors . Lens.at (ComponentName cname)
  let errorReport = fmap (Lens.view $ Errors.report . ErrorReport.user) <$> loggedError
  gassertEqual (Just target) errorReport

runSystemSpec :: Myo ()
runSystemSpec = do
  let ident = Str "cmd"
  myoAddSystemCommand $ AddSystemCommandOptions ident ["ls"] (Just runnerIdent) Nothing Nothing Nothing Nothing
  addDummyRunner runDummy
  myoRun ident
  check [testError]

test_runSystem :: IO ()
test_runSystem =
  specDef runSystemSpec

cmdline :: Text
cmdline = "echo 'hello"

lineRunner :: RunTask -> Myo ()
lineRunner (RunTask (Command _ _ lines' _ _ _ _) _ _) =
  reportError cname lines'

runLineSingleSpec :: Myo ()
runLineSingleSpec = do
  addDummyRunner lineRunner
  myoLine (RunLineOptions (Just cmdline) Nothing Nothing (Just runnerIdent) Nothing Nothing)
  check [cmdline]

test_runLineSingle :: IO ()
test_runLineSingle =
  specDef runLineSingleSpec

runLineCmdSpec :: Myo ()
runLineCmdSpec =
  vimCommand "MyoLine echo 'hello'"

test_runLineCmd :: IO ()
test_runLineCmd =
  intSpecDef runLineCmdSpec
