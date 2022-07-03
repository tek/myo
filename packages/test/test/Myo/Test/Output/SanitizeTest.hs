module Myo.Test.Output.SanitizeTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))

import Myo.Command.Interpreter.TmuxMonitor (sanitizeOutput)

outputData :: ByteString
outputData =
  "stack build --fast --pedantic\ESC[?2004l\ESC[29D\ESC[39m\ESC[24C\r\n\ESC[0mname-0.1.0.0: build (lib)\ESC[0m\n\ESC[0m"

test_sanitize :: UnitTest
test_sanitize =
  runTestAuto do
    "stack build --fast --pedantic\nname-0.1.0.0: build (lib)\n" === sanitizeOutput outputData
