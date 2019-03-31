{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.RunSpec(
  htf_thisModulesTests,
) where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Chiasma.Test.Tmux (sleep)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRun)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Tmux.Runner (addTmuxRunner)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

runSpec :: MyoN ()
runSpec = do
  addTmuxRunner
  initialize''
  myoAddSystemCommand $ AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) Nothing
  myoRun ident
  sleep 1
  output <- runTmux $ capturePane (PaneId 1)
  gassertElem line1 output
  gassertElem line2 output
  where
    ident = Str "cmd"
    cmds = T.unpack <$> ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]

test_tmuxRun :: IO ()
test_tmuxRun =
  tmuxGuiSpecDef runSpec
