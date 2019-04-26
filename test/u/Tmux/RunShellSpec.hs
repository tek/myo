{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.RunShellSpec (htf_thisModulesTests) where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(PaneId))
import qualified Control.Lens as Lens (at, view)
import Control.Monad.IO.Class (liftIO)
import qualified Ribosome.Data.ErrorReport as ErrorReport (user)
import Ribosome.Data.Errors (ComponentName(ComponentName))
import qualified Ribosome.Data.Errors as Errors (componentErrors, report)
import Ribosome.Error.Report (reportError)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Await (await)
import Ribosome.Test.Unit (withLog)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Config (defaultVars)
import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions(AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (running)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Kill (killCommand)
import Myo.Command.Run (myoRun)
import Myo.Command.Runner (addRunner, mkRunner)
import Myo.Command.RunningCommand (isCommandRunning)
import Myo.Data.Env (MyoN)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (setupDefaultTestUi)
import Unit (tmuxGuiSpecDef)

line1 :: Text
line1 =
  "shell command"

line2 :: Text
line2 =
  "with two cmdlines"

cmdLines :: [Text]
cmdLines =
  [line1, line2]

firstCondition ::
  AssertM m =>
  [Text] ->
  m ()
firstCondition output = do
  gassertEqual ("cat" : cmdLines) (take 3 output)
  gassertEqual cmdLines (drop 4 output)

secondCondition ::
  AssertM m =>
  [Text] ->
  m ()
secondCondition output =
  gassertEqual (cmdLines ++ cmdLines) (drop 6 output)

thirdCondition ::
  AssertM m =>
  [Text] ->
  m ()
thirdCondition output =
  gassertEqual (cmdLines ++ cmdLines) (drop 12 output)

tmuxRunShellSpec :: MyoN ()
tmuxRunShellSpec = do
  let shellIdent = "cat"
  let cmdIdent = "text"
  addTmuxRunner
  setupDefaultTestUi
  myoAddSystemCommand $ AddSystemCommandOptions shellIdent ["cat"] (Just "tmux") (Just "make") Nothing
  myoAddShellCommand $ AddShellCommandOptions cmdIdent cmdLines (Just "tmux") shellIdent Nothing
  myoRun cmdIdent
  await gassertBool (isCommandRunning shellIdent)
  await firstCondition paneContent
  myoRun cmdIdent
  await secondCondition paneContent
  killCommand shellIdent
  await (gassertBool . not) (isCommandRunning shellIdent)
  myoRun cmdIdent
  await gassertBool (isCommandRunning shellIdent)
  await thirdCondition paneContent
  where
    paneContent =
      runTmux $ capturePane (PaneId 1)

test_tmuxRunShell :: IO ()
test_tmuxRunShell =
  tmuxGuiSpecDef (withLog tmuxRunShellSpec)
