{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.RunSpec (htf_thisModulesTests) where

import Chiasma.Codec.Data.PaneMode (PaneMode(PaneMode))
import Chiasma.Command.Pane (capturePane, copyMode, paneMode)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Data.Text (Text)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Toggle (myoTogglePane)
import Unit (tmuxSpecDef)

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
  myoAddSystemCommand $ AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) Nothing Nothing
  where
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]

runAndCheck :: Myo ()
runAndCheck = do
  myoRun ident
  sleep 1
  output <- runTmux $ capturePane (PaneId 1)
  gassertElem line1 output
  gassertElem line2 output

runSysSpec :: Myo ()
runSysSpec = do
  setup
  runAndCheck

test_tmuxRunSys :: IO ()
test_tmuxRunSys =
  tmuxSpecDef runSysSpec

quitCopyModeSpec :: Myo ()
quitCopyModeSpec = do
  setup
  myoTogglePane "make"
  runTmux $ copyMode pid
  gassertEqual (Just (PaneMode pid "copy-mode")) =<< runTmux (paneMode pid)
  runAndCheck
  where
    pid =
      PaneId 1

test_quitCopyMode :: IO ()
test_quitCopyMode =
  tmuxSpecDef quitCopyModeSpec
