{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.ParseSpec(
  htf_thisModulesTests,
) where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Test.Tmux (sleep)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Test.Framework

import Config (vars)
import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Data.ParsedOutput (ParsedOutput)
import Myo.Command.Parse (addParser, myoParse)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (MyoN)
import Myo.Test.Unit (tmuxGuiSpec)
import Myo.Tmux.IO ()
import Myo.Tmux.Runner (addTmuxRunner)
import Test ()

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

lang :: CommandLanguage
lang = CommandLanguage "echo"

parseEcho :: [String] -> ParsedOutput
parseEcho = undefined

parseSpec :: MyoN ()
parseSpec = do
  let
    ident = Str "cmd"
    cmds = T.unpack <$> ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]
    opts = AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) (Just lang)
  addParser lang parseEcho
  addTmuxRunner
  myoAddSystemCommand opts
  myoRun ident
  sleep 2
  myoParse $ ParseOptions Nothing Nothing Nothing

test_parse :: IO ()
test_parse =
  vars >>= tmuxGuiSpec parseSpec
