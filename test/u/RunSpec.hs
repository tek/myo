{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (at, view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (gets)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner, mkRunner)
import Myo.Data.Env (MyoN)
import qualified Myo.Data.Env as Env (errors)
import Unit (specWithDef)

testError :: String
testError = "error"

cname :: String
cname = "test"

runDummy :: RunTask -> MyoN ()
runDummy _ =
  reportError cname [testError]

runSpec :: MyoN ()
runSpec = do
  let ident = Str "cmd"
  myoAddSystemCommand $ AddSystemCommandOptions ident ["ls"] (Just (Str "dummy")) Nothing Nothing
  _ <- addRunner (Str "dummy") (mkRunner runDummy) (const True)
  myoRun ident
  loggedError <- gets $ Lens.view $ Env.errors . Errors.componentErrors . Lens.at (ComponentName cname)
  let errorReport = fmap (Lens.view $ Errors.report . ErrorReport.user) <$> loggedError
  liftIO $ assertEqual (Just [testError]) errorReport

test_run :: IO ()
test_run =
  vars >>= specWithDef runSpec
