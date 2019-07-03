{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} Command.HistoryMenuSpec
import {-@ HTF_TESTS @-} Command.UpdateSpec
import {-@ HTF_TESTS @-} Command.VimTestSpec
import {-@ HTF_TESTS @-} ConfigSpec
import {-@ HTF_TESTS @-} DiagSpec
import {-@ HTF_TESTS @-} Output.CycleSpec
import {-@ HTF_TESTS @-} Output.EmptyOutputSpec
import {-@ HTF_TESTS @-} Output.HaskellRenderSpec
import {-@ HTF_TESTS @-} Output.ParseHaskellSpec
import {-@ HTF_TESTS @-} Output.ParseScalaSpec
import {-@ HTF_TESTS @-} Output.QuitSpec
import {-@ HTF_TESTS @-} Output.SanitizeSpec
import {-@ HTF_TESTS @-} Output.ScalaRenderSpec
import {-@ HTF_TESTS @-} Output.SelectSpec
import {-@ HTF_TESTS @-} ProcSpec
import {-@ HTF_TESTS @-} RunSpec
import {-@ HTF_TESTS @-} SaveSpec
import {-@ HTF_TESTS @-} SocketSpec
import {-@ HTF_TESTS @-} Tmux.FocusSpec
import {-@ HTF_TESTS @-} Tmux.ParseSpec
import {-@ HTF_TESTS @-} Tmux.RunShellSpec
import {-@ HTF_TESTS @-} Tmux.RunSpec
import {-@ HTF_TESTS @-} Tmux.ToggleLayoutSpec
import {-@ HTF_TESTS @-} Tmux.TogglePaneSpec
import {-@ HTF_TESTS @-} Tmux.UpdateSpec

main :: IO ()
main = htfMain htf_importedTests
