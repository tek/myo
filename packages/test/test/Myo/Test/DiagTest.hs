module Myo.Test.DiagTest where

import Polysemy.Test (UnitTest, assertEq)
import Ribosome.Api.Buffer (currentBufferContent)

import Myo.Diag (myoDiag)
import Myo.Interpreter.Commands (interpretCommandsNoHistory)
import Myo.Test.Embed (myoTest)

target :: [Text]
target = [
  "# Diagnostics",
  "",
  "## Commands",
  "",
  "",
  "## Ui",
  "",
  ""
  ]

test_diag :: UnitTest
test_diag =
  myoTest $ interpretCommandsNoHistory do
    subsume myoDiag
    assertEq target =<< currentBufferContent
