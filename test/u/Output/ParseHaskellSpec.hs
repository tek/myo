{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.ParseHaskellSpec(
  htf_thisModulesTests,
) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Attoparsec.Text (parseOnly)
import Data.Foldable (traverse_)
import Data.Functor.Syntax
import Data.Text (Text)
import qualified Data.Text as Text (unlines, unpack)
import Data.Word (Word8)
import Myo.Output.Data.OutputEvent (OutputEvent)
import Test.Framework
import Text.Parser.Char (CharParsing, anyChar, char, digit, newline, notChar, string)

import Myo.Command.Parse (parseWith)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParseReport as ParseReport (_lines)
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine (_text)
import Myo.Output.Lang.Haskell.Parser hiding (parseHaskell)

haskellOutput :: Text
haskellOutput =
  Text.unlines [
    -- "/path/to/Module/File.hs:13:5: error:",
    -- "    • Couldn't match type ‘TypeA’ with ‘TypeB’",
    -- "      Expected type: TypeB",
    -- "        Actual type: TypeA",
    -- "    • In the expression: f a b",
    -- "      In an equation for ‘Module.foo’:",
    -- "          foo = f a b",
    -- "   |",
    -- "13 |   f a b",
    -- "   |   ^^^^",
    -- "",
    -- "/path/to/Module/File.hs:19:7: error: [-fhygiene]",
    -- "    Not in scope: type constructor or class ‘Something’",
    -- "   |",
    -- "19 | makeSomething :: String -> [Something]",
    -- "   |                             ^^^^^^^^^",
    -- "",
    -- "/path/to/Module/File.hs:2:77: warning: [-Wmissing-methods]",
    -- "    • No explicit implementation for",
    -- "        ‘meth’",
    -- "    • In the instance declaration for ‘Closs Int’",
    -- "   |",
    -- "31 | instance Closs Int where",
    -- "   |          ^^^^^^^^^",
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

parseHaskell :: IO (Either OutputError ParsedOutput)
parseHaskell =
  runExceptT $ parseWith haskellOutputParser haskellOutput

test_parseHaskell :: IO ()
test_parseHaskell = do
  outputE <- parseHaskell
  ParsedOutput cons <- assertRight outputE
  let
    report = cons 0
    lines = Text.unpack . ReportLine._text <$> ParseReport._lines report
  traverse_ putStrLn lines
