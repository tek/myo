module Myo.Test.Backend where

import qualified Data.Map.Strict as Map
import Log (Severity (Error))
import Polysemy.Test (Hedgehog, assertJust)
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
import qualified Ribosome.Report as Report

import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Effect.Backend (Backend)
import Myo.Command.Interpreter.Backend.Generic (captureUnsupported, interceptBackend)

newtype RunTestError =
  RunTestError { unTestError :: [Text] }
  deriving stock (Eq, Show)
  deriving newtype (Ord)

instance Reportable RunTestError where
  toReport (RunTestError e) =
    Report (unlines e) e Error

ctx :: ReportContext
ctx = "test"

testError :: Text
testError =
  "error"

checkReport ::
  Members [Hedgehog IO, Reports] r =>
  [Text] ->
  Sem r ()
checkReport target = do
  loggedErrors <- Map.lookup ctx <$> storedReports
  assertJust target (user <$> (head =<< loggedErrors))
  where
    user (StoredReport (Report _ l _) _) = l

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
