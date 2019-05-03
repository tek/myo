{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (at, view)
import Ribosome.Control.Monad.Ribo (inspectErrors)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner, mkRunner)
import Myo.Data.Env (Myo)
import Unit (specDef)

testError :: Text
testError = "error"

cname :: Text
cname = "test"

runDummy :: RunTask -> Myo ()
runDummy _ =
  reportError cname [testError]

runSystemSpec :: Myo ()
runSystemSpec = do
  let ident = Str "cmd"
  myoAddSystemCommand $ AddSystemCommandOptions ident ["ls"] (Just (Str "dummy")) Nothing Nothing
  addRunner (Str "dummy") (mkRunner runDummy) (const True)
  myoRun ident
  loggedError <- inspectErrors $ Lens.view $ Errors.componentErrors . Lens.at (ComponentName cname)
  let errorReport = fmap (Lens.view $ Errors.report . ErrorReport.user) <$> loggedError
  liftIO $ assertEqual (Just [testError]) errorReport

test_runSystem :: IO ()
test_runSystem =
  specDef runSystemSpec
