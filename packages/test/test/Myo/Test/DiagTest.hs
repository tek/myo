module Myo.Test.DiagTest where

import Ribosome.Api.Buffer (currentBufferContent)

import Myo.Diag (myoDiag)
import Polysemy.Test (UnitTest, assertEq)
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
  myoTest do
    myoDiag
    assertEq target =<< currentBufferContent
