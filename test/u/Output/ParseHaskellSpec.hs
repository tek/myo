{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.ParseHaskellSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Test.Framework

import Myo.Command.Parse (parseWith)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParseReport as ParseReport (_lines)
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine (_text)
import Myo.Output.Lang.Haskell.Parser hiding (parseHaskell)

haskellOutput :: Text
haskellOutput =
  Text.unlines [
    "leading crap",
    "/path/to/Module/File.hs:13:5: error:",
    "    • Couldn't match type ‘TypeA’ with ‘TypeB’",
    "      Expected type: TypeB",
    "        Actual type: TypeA",
    "    • In the expression: f a b",
    "      In an equation for ‘Module.foo’:",
    "          foo = f a b",
    "   |",
    "13 |   f a b",
    "   |   ^^^^",
    "",
    "/path/to/Module/File.hs:19:7: error: [-fhygiene]",
    "    Not in scope: type constructor or class ‘Something’",
    "   |",
    "19 | makeSomething :: Text -> [Something]",
    "   |                             ^^^^^^^^^",
    "",
    "/path/to/Module/File.hs:2:77: warning: [-Wmissing-methods]",
    "    • No explicit implementation for",
    "        ‘meth’",
    "    • In the instance declaration for ‘Closs Int’",
    "   |",
    "31 | instance Closs Int where",
    "   |          ^^^^^^^^^",
    "",
    "/path/to/file.hs:5:5: error:",
    "    • Couldn't match expected type ‘StateT",
    "                                      (ReaderT",
    "                                         (GHC.Conc.Sync.TVar Data))",
    "                                      a0’",
    "                  with actual type ‘t0 Data1",
    "                                    -> StateT (ReaderT e0) ()’",
    "    • Probable cause: ‘mapM_’ is applied to too few arguments",
    "      In a stmt of a 'do' block: mapM_ end",
    "      In the expression:",
    "        do a <- start",
    "           mapM_ end",
    "      In an equation for ‘execute’:",
    "          execute",
    "            = do a <- start",
    "                 mapM_ end",
    "  |",
    "5 |   mapM_ end",
    "  |   ^^^^^^^^^",
    ""
    ]

target :: Vector Text
target = Vector.fromList [
  "/path/to/Module/File.hs \57505 14",
  "type mismatch",
  "TypeA",
  "TypeB",
  "",
  "/path/to/Module/File.hs \57505 20",
  "type not in scope: Something",
  "",
  "/path/to/Module/File.hs \57505 3",
  "method not implemented: meth",
  "",
  "/path/to/file.hs \57505 6",
  "type mismatch",
  "t0 Data1 -> StateT (ReaderT e0) ()",
  "StateT (ReaderT (GHC.Conc.Sync.TVar Data)) a0",
  ""
  ]

parseHaskell :: IO (Either OutputError ParsedOutput)
parseHaskell =
  runExceptT $ parseWith haskellOutputParser haskellOutput

test_parseHaskell :: IO ()
test_parseHaskell = do
  outputE <- parseHaskell
  ParsedOutput _ cons <- assertRight outputE
  let
    report = cons 0
    lines' = ReportLine._text <$> ParseReport._lines report
  assertEqual target lines'
