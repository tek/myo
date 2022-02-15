module Myo.Test.Output.ParseTest where

import Control.Lens (view)
import qualified Data.Vector as Vector
import Hedgehog ((/==))
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (fixture)

import Myo.Command.Log (appendLog, pushCommandLog, commandLogs)
import Myo.Command.Parse (parseCommand, selectCommand)
import Myo.Command.Run (myoRunIdent)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (events)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Output.Echo (addEchoCommand, addEchoHandler)
import Myo.Test.Unit (MyoTest, tmuxTestDef)
import Ribosome.Test.Await (awaitEqual)

lines' :: [Text]
lines' =
  ["line1"]

parsePreviousTest :: MyoTest ()
parsePreviousTest = do
  lift addSubprocessRunner
  lift . addEchoHandler =<< fixture "tmux/parse/file"
  ident <- lift (addEchoCommand "proc" lines' False)
  lift (myoRunIdent ident)
  awaitEqual 1 length commandLogs
  cmd <- selectCommand (Just ident)
  pushCommandLog ident
  appendLog ident "unparsable"
  outputEvents <- fmap (view ParsedOutput.events) <$> parseCommand cmd
  let ParseReport events _ = compileReport 0 (fold outputEvents)
  Vector.empty /== events

test_parsePrevious :: UnitTest
test_parsePrevious =
  tmuxTestDef parsePreviousTest
