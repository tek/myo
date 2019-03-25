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
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRun)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Test.Unit (tmuxGuiSpec)
import Myo.Tmux.IO (runTmuxE)
import Myo.Tmux.Runner (addTmuxRunner)
import Test ()

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

runSpec :: MyoN ()
runSpec = do
  let ident = Str "cmd"
  let cmds = T.unpack <$> ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]
  addTmuxRunner
  initialize''
  let opts = AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) Nothing
  myoAddSystemCommand opts
  myoRun ident
  sleep 1
  outputE <- runTmuxE $ capturePane (PaneId 1)
  output <- gassertRight outputE
  gassertElem line1 output
  gassertElem line2 output

test_tmuxRun :: IO ()
test_tmuxRun =
  vars >>= tmuxGuiSpec runSpec
