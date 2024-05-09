module Myo.Test.CommandTest where

import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Myo.Test.Command.CommandMenuTest (test_commandMenu)
import Myo.Test.Command.CommandSpecTest (test_commandSpec)
import Myo.Test.Command.HistoryMenuTest (test_historyMenu, test_historyMenuDelete, test_historyMenuEdit)
import Myo.Test.Command.HistoryTest (test_history)
import Myo.Test.Command.TestTest (test_testCommand, test_testCommandDefault, test_testCommandEmpty)
import Myo.Test.Command.UpdateTest (test_updateCommands)
import Myo.Test.Command.VimTestTest (test_vimTest)
import Myo.Test.RunTest (
  test_runLineSingle,
  test_runParamCommand,
  test_runParamCommandOptparse,
  test_runSubprocFail,
  test_runSystem,
  )

commandTests :: TestTree
commandTests =
  testGroup "command" [
    unitTest "run a command from the menu" test_commandMenu,
    test_history,
    testGroup "history menu" [
      unitTest "select a command" test_historyMenu,
      unitTest "delete some entries" test_historyMenuDelete,
      unitTest "edit an entry" test_historyMenuEdit
    ],
    unitTest "update commands via variable watcher" test_updateCommands,
    testGroup "run" [
      unitTest "vim-test command" test_vimTest,
      unitTest "command with the system runner" test_runSystem,
      unitTest "single ad-hoc cmdline" test_runLineSingle,
      unitTest "failing command with the subproc runner" test_runSubprocFail,
      unitTest "command with parameters" test_runParamCommand,
      unitTest "test command with optparse args" test_runParamCommandOptparse,
      unitTest "test command" test_testCommand,
      unitTest "test command with custom command with defaults" test_testCommandDefault,
      unitTest "test command with empty cmdline" test_testCommandEmpty
    ],
    test_commandSpec
  ]
