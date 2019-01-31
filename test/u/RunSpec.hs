{-# OPTIONS_GHC -F -pgmF htfpp #-}

module RunSpec(
  htf_thisModulesTests,
) where

import qualified Control.Lens as Lens (view, at, preview, each)
import Chiasma.Data.Ident (Ident(Str))
import Control.Monad.IO.Class (liftIO)
import qualified Ribosome.Control.Ribo as Ribo (inspect)
import Ribosome.Control.Monad.RiboE (runRiboE, liftRibo)
import Ribosome.Control.Monad.State (riboState, runRiboState)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (_componentErrors, _errorMessage)
import Ribosome.Msgpack.NvimObject (NO(..))
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Myo, MyoE, Runner, Pid)
import qualified Myo.Data.Env as Env (_errors)
import Myo.Env (logError)
import Myo.Test.Unit (specWithDef)

testError :: String
testError = "error"

cname :: ComponentName
cname = ComponentName "test"

runDummy :: RunTask -> MyoE RunError (Maybe Pid)
runDummy _ = do
  liftRibo $ logError cname [testError]
  return Nothing

runSpec :: Myo ()
runSpec = do
  let ident = Str "cmd"
  myoAddSystemCommand $ NO $ AddSystemCommandOptions ident ["ls"] (Just (Str "dummy")) Nothing
  _ <- runRiboState $ addRunner (Str "dummy") runDummy (const True)
  myoRun (NO ident)
  loggedError <- Ribo.inspect $ Lens.view $ Env._errors . Errors._componentErrors . Lens.at cname
  let errorMessage = fmap (Lens.view Errors._errorMessage) <$> loggedError
  liftIO $ assertEqual (Just [[testError]]) errorMessage

test_run :: IO ()
test_run =
  vars >>= specWithDef runSpec
