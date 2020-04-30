{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.ParseSpec (htf_thisModulesTests) where

import Control.Lens (view)
import qualified Data.Vector as Vector (toList)
import Test.Framework

import Myo.Command.Log (appendLog, pushCommandLog)
import Myo.Command.Parse (parseCommand, selectCommand)
import Myo.Command.Run (myoRunIdent)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Data.Env (Myo)
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (events)
import Myo.Output.ParseReport (compileReport)
import Output.Echo (addEchoCommand, addEchoHandler)
import Unit (specDef)

lines' :: [Text]
lines' =
  ["line1"]

parsePreviousSpec :: Myo ()
parsePreviousSpec = do
  addSubprocessRunner
  addEchoHandler =<< fixture "tmux/parse/file"
  ident <- addEchoCommand "proc" lines'
  myoRunIdent ident
  cmd <- selectCommand (Just ident)
  pushCommandLog ident
  appendLog ident "unparsable"
  outputEvents <- view ParsedOutput.events <$$> parseCommand cmd
  let ParseReport events _ = compileReport 0 (fold outputEvents)
  gassertNotEmpty (Vector.toList events)

test_parsePrevious :: IO ()
test_parsePrevious =
  specDef parsePreviousSpec
