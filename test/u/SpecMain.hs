{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()
import {-@ HTF_TESTS @-} DiagSpec
import {-@ HTF_TESTS @-} Output.HaskellRenderSpec
import {-@ HTF_TESTS @-} Output.ParseHaskellSpec
import {-@ HTF_TESTS @-} ProcSpec
import {-@ HTF_TESTS @-} RunSpec
import {-@ HTF_TESTS @-} SocketSpec
import {-@ HTF_TESTS @-} Tmux.ParseSpec
import {-@ HTF_TESTS @-} Tmux.RunSpec
import {-@ HTF_TESTS @-} ToggleLayoutSpec
import {-@ HTF_TESTS @-} TogglePaneSpec

main :: IO ()
main = htfMain htf_importedTests
