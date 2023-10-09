module Myo.Test.Output.EmptyOutputTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimListWins)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Output (compileAndRenderReport)
import Myo.Interpreter.Commands (interpretCommandsNoHistory)
import Myo.Test.Embed (myoTest)

test_emptyOutput :: UnitTest
test_emptyOutput =
  myoTest $ interpretCommandsNoHistory do
    runStop (resume_ @CommandError compileAndRenderReport) >>= \case
      Right () -> unit
      Left _ -> unit
    wins <- nvimListWins
    1 === length wins
