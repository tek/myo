module Myo.Test.Output.EmptyOutputTest where

import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimListWins)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Interpreter.Outputs (interpretOutputs)
import Myo.Test.Embed (myoTest)

test_emptyOutput :: UnitTest
test_emptyOutput =
  myoTest $ interpretOutputs do
    runStop compileAndRenderReport >>= \case
      Right () -> unit
      Left _ -> unit
    wins <- nvimListWins
    1 === length wins
