module Myo.Test.SaveTest where

import Conc (interpretAtomic)
import Polysemy.Test (UnitTest, assertEq)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Data.CommandId (CommandId)
import Myo.Data.LastSave (LastSave)
import Myo.Save (myoSave)
import qualified Myo.Settings as Settings (saveInterval)
import Myo.Test.Embed (myoTest)

ident :: CommandId
ident =
  "cmd"

line :: Text
line =
  "log line"

getOutput ::
  Member CommandLog r =>
  Sem r (Maybe Text, Maybe Text)
getOutput = do
  (,) <$> CommandLog.get ident <*> CommandLog.getPrev ident

pushOutput ::
  Member CommandLog r =>
  Sem r ()
pushOutput =
  CommandLog.append ident "log line"

type SaveTestStack =
  [Sync (), CommandLog, AtomicState LastSave]

test_save :: UnitTest
test_save =
  myoTest $ interpretAtomic (def :: LastSave) $ interpretCommandLogSetting $ testHandler do
    Settings.update Settings.saveInterval 0
    myoAddSystemCommand (AddSystemCommandOptions.cons ident [])
    myoSave
    assertWait getOutput (assertEq (Nothing, Nothing))
    pushOutput
    assertWait getOutput (assertEq (Just line, Nothing))
    myoSave
    assertWait getOutput (assertEq (Just "", Just line))
    myoSave
    assertWait getOutput (assertEq (Just "", Just line))
    pushOutput
    void (CommandLog.get ident)
    pushOutput
    assertWait getOutput (assertEq (Just (line <> line), Just line))
    myoSave
    assertWait getOutput (assertEq (Just "", Just (line <> line)))
