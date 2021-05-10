module Main where

import Myo.Test.Command.CommandMenuTest (test_commandMenu)
import Myo.Test.Command.HistoryMenuTest (test_historyMenu)
import Myo.Test.Command.UpdateTest (test_updateCommands)
import Myo.Test.Command.VimTestTest (test_vimTest)
import Myo.Test.CompleteTest (test_completeCommand)
import Myo.Test.ConfigTest (test_proteomeConfig)
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
import Myo.Test.ProcTest (test_childPids, test_ppid)
import Myo.Test.RunTest (test_runLineCmd, test_runLineSingle, test_runSubproc, test_runSystem)
import Myo.Test.SaveTest (test_save)
import Myo.Test.SocketTest (test_socket)
import Myo.Test.Tmux.FocusTest (test_focusPane)
import Myo.Test.Tmux.ParseTest (test_parseCaptureTmux, test_parseTmux)
import Myo.Test.Tmux.RunShellTest (test_tmuxRunShell)
import Myo.Test.Tmux.RunTest (test_quitCopyMode, test_tmuxRunSys)
import Myo.Test.Tmux.ToggleLayoutTest (test_toggleLayout)
import Myo.Test.Tmux.TogglePaneTest (test_shellPanePin, test_togglePane)
import Myo.Test.Tmux.UpdateTest (test_updateUi)
import Ribosome.Test.Run (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "run a command from the menu" test_commandMenu,
    unitTest "select a command in the history menu" test_historyMenu,
    unitTest "update commands via variable watcher" test_updateCommands,
    unitTest "run a vim-test command" test_vimTest,
    unitTest "command completion" test_completeCommand,
    unitTest "react to proteome being loaded" test_proteomeConfig,
    unitTest "diagnostics window" test_diag,
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
    unitTest "select an event in the output window" test_outputSelect,
    unitTest "determine the parent PID" test_ppid,
    unitTest "determine child PIDs" test_childPids,
    unitTest "run a command with the system runner" test_runSystem,
    unitTest "run a single ad-hoc cmdline" test_runLineSingle,
    unitTest "run a single ad-hoc cmdline via command" test_runLineCmd,
    unitTest "run a command with the subproc runner" test_runSubproc,
    unitTest "push output entries on save" test_save,
    unitTest "socket communication" test_socket,
    unitTest "focus a pane" test_focusPane,
    unitTest "parse output from tmux" test_parseTmux,
    unitTest "parse output from tmux by capturing the current pane content" test_parseCaptureTmux,
    unitTest "run a shell command in tmux" test_tmuxRunShell,
    unitTest "run a system command in tmux" test_tmuxRunSys,
    unitTest "quit the tmux copy mode" test_quitCopyMode,
    unitTest "toggle a tmux layout" test_toggleLayout,
    unitTest "toggle a tmux pane" test_togglePane,
    unitTest "pin a shell pane in tmux" test_shellPanePin,
    unitTest "update the UI config via variable watcher" test_updateUi
  ]


main :: IO ()
main =
  defaultMain tests
