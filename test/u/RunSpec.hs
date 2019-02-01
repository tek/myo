{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (Ident(Str))
import qualified Control.Lens as Lens (view, at, preview, each)
import Control.Monad.IO.Class (liftIO)
import Ribosome.Control.Monad.RiboE (runRiboE, liftRibo)
import Ribosome.Control.Monad.State (riboState, runRiboStateE)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Ribosome.Msgpack.NvimObject (NO(..))
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Myo, MyoE, Runner)
import qualified Myo.Data.Env as Env (_errors)
import Myo.Test.Unit (specWithDef)

testError :: String
testError = "error"

cname :: String
cname = "test"

runDummy :: RunTask -> MyoE RunError (Maybe Pid)
runDummy _ = do
  liftRibo $ reportError cname [testError]
  return Nothing

runSpec :: Myo ()
runSpec = do
  let ident = Str "cmd"
  myoAddSystemCommand $ NO $ AddSystemCommandOptions ident ["ls"] (Just (Str "dummy")) Nothing
  _ <- runRiboStateE $ addRunner (Str "dummy") runDummy (const True)
  myoRun (NO ident)
  loggedError <- Ribo.inspect $ Lens.view $ Env._errors . Errors.componentErrors . Lens.at (ComponentName cname)
  let errorReport = fmap (Lens.view $ Errors.report . ErrorReport.user) <$> loggedError
  liftIO $ assertEqual (Just [testError]) errorReport

test_run :: IO ()
test_run =
  vars >>= specWithDef runSpec
