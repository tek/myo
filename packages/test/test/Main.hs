module Main where

import Myo.Test.Command.CommandMenuTest (test_commandMenu)
import Myo.Test.Command.HistoryMenuTest (test_historyMenu)
import Myo.Test.Command.UpdateTest (test_updateCommands)
import Myo.Test.Command.VimTestTest (test_vimTest)
import Myo.Test.CompleteTest (test_completeCommand)
import Myo.Test.DiagTest (test_diag)
import Myo.Test.Output.CycleTest (test_outputNext, test_outputPrev)
import Myo.Test.Output.EmptyOutputTest (test_emptyOutput)
import Myo.Test.Output.HaskellRenderTest (test_haskellRender)
import Myo.Test.Output.ParseHaskellTest (test_parseGarbage, test_parseHaskellErrors)
import Myo.Test.Output.ParseScalaTest (test_parseScala)
import Myo.Test.Output.ParseTest (test_parsePrevious)
import Myo.Test.Output.PathTest (test_outputResolvePath)
import Myo.Test.Output.QuitTest (test_outputQuit)
import Myo.Test.Output.SanitizeTest (test_sanitize)
import Myo.Test.Output.ScalaRenderTest (test_scalaRender)
import Myo.Test.Output.SelectTest (test_outputSelect)
import Myo.Test.ProcTest (test_proc)
import Myo.Test.RunTest (test_runLineSingle, test_runSystem, test_runSubprocFail)
import Myo.Test.SaveTest (test_save)
import Myo.Test.Tmux.CommandLogTest (test_tmuxTruncCommandLog)
import Myo.Test.Tmux.FocusTest (test_focusPane)
import Myo.Test.Tmux.KillTest (test_tmuxKill)
import Myo.Test.Tmux.ParseTest (test_parseCaptureTmux, test_parseTmux)
import Myo.Test.Tmux.RunShellTest (test_tmuxRunShell)
import Myo.Test.Tmux.RunTest (test_quitCopyMode, test_tmuxRunSys)
import Myo.Test.Tmux.ToggleLayoutTest (test_toggleLayout)
import Myo.Test.Tmux.TogglePaneTest (test_shellPanePin, test_togglePane)
import Myo.Test.Tmux.UpdateTest (test_updateUi)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Polysemy.Test (unitTest)

tests :: TestTree
tests =
  testGroup "all" [
    testGroup "command" [
      unitTest "run a command from the menu" test_commandMenu,
      unitTest "select a command in the history menu" test_historyMenu,
      unitTest "update commands via variable watcher" test_updateCommands,
      unitTest "run a vim-test command" test_vimTest
    ],
    testGroup "output" [
      unitTest "cycle to the previous output event" test_outputPrev,
      unitTest "cycle to the next output event" test_outputNext,
      unitTest "don't show the output window if events are empty" test_emptyOutput,
      unitTest "render haskell messages" test_haskellRender,
      unitTest "parse haskell errors" test_parseHaskellErrors,
      unitTest "parse haskell garbage" test_parseGarbage,
      unitTest "parse scala messages" test_parseScala,
      unitTest "parse previous output if current is unparsable" test_parsePrevious,
      unitTest "find a file from an output message" test_outputResolvePath,
      unitTest "quit the output window" test_outputQuit,
      unitTest "sanitize output messages" test_sanitize,
      unitTest "render scala messages" test_scalaRender,
      unitTest "select an event in the output window" test_outputSelect
    ],
    testGroup "tmux" [
      unitTest "truncate the command log for a tmux command" test_tmuxTruncCommandLog,
      unitTest "focus a pane" test_focusPane,
      unitTest "tmux: kill running command" test_tmuxKill,
      unitTest "parse output from tmux" test_parseTmux,
      unitTest "parse output from tmux by capturing the current pane content" test_parseCaptureTmux,
      unitTest "run a shell command in tmux" test_tmuxRunShell,
      unitTest "run a system command in tmux" test_tmuxRunSys,
      unitTest "quit the tmux copy mode" test_quitCopyMode,
      unitTest "toggle a tmux layout" test_toggleLayout,
      unitTest "toggle a tmux pane" test_togglePane,
      unitTest "pin a shell pane in tmux" test_shellPanePin,
      unitTest "update the UI config via variable watcher" test_updateUi
    ],
    unitTest "command completion" test_completeCommand,
    unitTest "diagnostics window" test_diag,
    unitTest "determine parent and child pids" test_proc,
    unitTest "run a command with the system runner" test_runSystem,
    unitTest "run a single ad-hoc cmdline" test_runLineSingle,
    unitTest "run a failing command with the subproc runner" test_runSubprocFail,
    unitTest "push output entries on save" test_save
  ]


main :: IO ()
main =
  defaultMain tests
