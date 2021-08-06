module Myo.Test.Tmux.RunTest where

import Chiasma.Codec.Data.PaneMode (PaneMode (PaneMode))
import Chiasma.Command.Pane (capturePane, copyMode, paneMode)
import Chiasma.Data.Ident (Ident (Str))
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Hedgehog ((===))
import Ribosome.Test.Await (awaitEqual)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Tmux.Run (runTmux)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Test.Unit (MyoTest, tmuxTestDef)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Toggle (myoTogglePane)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

ident :: Ident
ident = Str "cmd"

setup :: Myo ()
setup = do
  addTmuxRunner
  initialize''
  myoAddSystemCommand cmd
  where
    cmd =
      AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) Nothing Nothing Nothing Nothing Nothing
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]

-- output may be interleaved, so the `line 2` might be prefixed by `bash-4.4 $`
runAndCheck :: MyoTest ()
runAndCheck = do
  lift (myoRun (identText ident))
  awaitEqual True checks (runTmux (cleanLines <$> capturePane (PaneId 1)))
  where
    checks output =
      check line1 output && check line2 output
    check line =
      any ((==) line)

runSysTest :: MyoTest ()
runSysTest = do
  lift setup
  runAndCheck

test_tmuxRunSys :: UnitTest
test_tmuxRunSys =
  tmuxTestDef runSysTest

quitCopyModeTest :: MyoTest ()
quitCopyModeTest = do
  lift do
    setup
    myoTogglePane "make"
    runTmux (copyMode pid)
  (Just (PaneMode pid "copy-mode") ===) =<< lift (runTmux (paneMode pid))
  runAndCheck
  where
    pid =
      PaneId 1

test_quitCopyMode :: UnitTest
test_quitCopyMode =
  tmuxTestDef quitCopyModeTest
