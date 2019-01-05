{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework
import Test.Framework.BlackBoxTest ()

main :: IO ()
main =
  return ()
  -- htfMain htf_importedTests
