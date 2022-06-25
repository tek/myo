module Myo.Test.DiagTest where

import Ribosome.Api.Buffer (currentBufferContent)

import Myo.Diag (myoDiag)

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

diagTest :: Sem r ()
diagTest = do
  myoDiag
  content <- currentBufferContent
  target === content

test_diag :: UnitTest
test_diag =
  myoTest diagTest
