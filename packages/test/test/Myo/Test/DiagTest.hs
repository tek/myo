module Myo.Test.DiagTest where

import Hedgehog ((===))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Test.Run (UnitTest)

import Myo.Diag (myoDiag)
import Myo.Test.Unit (MyoTest, testDef)

target :: [Text]
target = [
  "# Diagnostics",
  "",
  "## Commands",
  "",
  "",
  "## Ui",
  "",
  "",
  "## Errors"
  ]

diagTest :: MyoTest ()
diagTest = do
  myoDiag
  content <- currentBufferContent
  target === content

test_diag :: UnitTest
test_diag =
  testDef diagTest
