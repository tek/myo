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
import Ribosome.Msgpack.NvimObject ((-$))
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import Myo.Test.Unit (tmuxGuiSpecWithDef)
import Myo.Tmux.IO (liftTmux)
import Myo.Tmux.Runner (addTmuxRunner)
import Test ()

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

runSpec :: Myo ()
runSpec = do
  let ident = Str "cmd"
  let cmds = T.unpack <$> ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]
  addTmuxRunner
  let opts = AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make"))
  myoAddSystemCommand -$ opts
  myoRun -$ ident
  sleep 2
  outputE <- liftTmux $ capturePane (PaneId 1)
  output <- gassertRight outputE
  gassertElem line1 output
  gassertElem line2 output

test_tmuxRun :: IO ()
test_tmuxRun =
  vars >>= tmuxGuiSpecWithDef runSpec
