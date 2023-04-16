module Myo.Test.Output.ParseHaskellTest where

import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Exon (exon)
import Path (relfile)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, TestError, UnitTest, (===))
import Ribosome.Test (testError)

import qualified Myo.Output.Data.ParseReport as ParseReport
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Embed (myoTest)

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
    "/path/to/File.hs:5:5: error:",
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
    "",
    "    /path/to/File.hs:17:1: error: [-Wunused-imports, -Werror=unused-imports]",
    "        The import of ‘ComposeSt, captureT, control, defaultLiftBaseWith,",
    "                       defaultRestoreM, embed, embed_’",
    "        from module ‘Control.Monad.Trans.Control’ is redundant",
    "       |",
    "    17 | import Control.Monad.Trans.Control (ComposeSt, MonadBaseControl(..), defaultRestoreM)",
    "       | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "",
    "    /path/to/File.hs:24:1: error: [-Wunused-imports, -Werror=unused-imports]",
    "        The import of ‘Data.Either.Combinators’ is redundant",
    "          except perhaps to import instances from ‘Data.Either.Combinators’",
    "        To import instances alone, use: import Data.Either.Combinators()",
    "       |",
    "    24 | import Data.Either.Combinators (swapEither)",
    "       | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^",
    "",
    "    /path/to/File.hs:36:1: error:",
    "        Parse error: module header, import declaration",
    "        or top-level declaration expected.",
    "       |",
    "    36 | xyzabcd",
    "       | ^^^^^^^",
    "",
    "    /path/to/File.hs:36:1: error:",
    "        • No instance for (MonadIO (t m)) arising from a use of ‘func’",
    "          Possible fix:",
    "            add (MonadIO (t m)) to the context of",
    "              the type signature for:",
    "                func :: forall a (m :: * -> *). a -> m ()",
    "        • In the expression: func",
    "          In an equation for ‘func’: func = liftIO",
    "       |",
    "    70 |   func",
    "       |   ^^^^",
    "",
    "    /path/to/File.hs:36:1: error:",
    "      Variable not in scope: var",
    "        :: IO a0",
    "       |",
    "    77 |   var",
    "       |   ^^^",
    "    /path/to/File.hs:36:1: error:",
    "        The qualified import of ‘Name’",
    "        from module ‘Module’ is redundant",
    "       |",
    "    14 | import qualified Module as M (",
    "       | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    A do-notation statement discarded a result of type",
    "      ‘m ()’",
    "    Suppress this warning by saying",
    "      ‘_ <- expr’",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    Module ‘Mod.Ule’ does not export ‘NoSuchName’",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    File name does not match module name:",
    "    Saw: ‘Wrong.Module’",
    "    Expected: ‘Correct.Module’",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    Could not find module ‘Non.Existing.Module’",
    "    Use -v to see a list of the files searched for.",
    "",
    "    /path/to/File.hs:36:1: error:",
    "      • Could not deduce (Class t m)",
    "        arising from a use of ‘constrainedFun’",
    "      from the context: (MonadIO m, MonadTrans t)",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    • Couldn't match type ‘Found’",
    "                     with ‘Req’",
    "      Expected type: Param -> Req",
    "        Actual type: Param",
    "                     -> Found",
    "",
    "    /path/to/File.hs:36:1: error:",
    "    • Ambiguous type variable ‘a0’ arising from a use of ‘funky’",
    "      prevents the constraint ‘(Classy a0)’ from being solved.",
    "",
    "/path/to/File.hs:36:1-10: error:",
    "    Not in scope: ‘Mod.name’",
    "    Module ‘Mod’ does not export ‘name’.",
    "",
    "assertEqual failed at path/to/Test.hs:53",
    "",
    "test-suite: Prelude.undefined",
    "CallStack (from HasCallStack):",
    "  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err",
    "  undefined, called at /path/to/File.hs:36:34 in package:File",
    "user error (test exceeded timeout of 10 seconds)",
    "",
    "Prelude.undefined",
    "CallStack:",
    "  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err",
    "  undefined, called at /path/to/File.hs:37:34 in package:File",

    "",
    "/path/to/File.hs:(999,1)-(1140,43): Non-exhaustive patterns in function wrong",
    "",
    "/path/to/File.hs:36:1: error:",
    "Could not deduce (IsString a1) arising from the literal ‘\"to\"’",
    "from the context: (Monad m)",
    "",
    "  module-name      > /path/to/File.hs:36:1: error:",
    "  module-name      >   Not in scope: type constructor or class ‘Something’",
    "module-name      >      |",
    "module-name      >   19 | makeSomething :: Text -> [Something]",
    "module-name      >      |                           ^^^^^^^^^",
    "",
    "/path/to/File.hs:36:1: error:",
    "Not in scope: data constructor ‘Dat’",
    "",
    "/path/to/File.hs:(36,1)-(39,55): error:",
    "",
    "Pattern match(es) are non-exhaustive",
    "  In an equation for ‘nonex’:",
    "",
    [exon|
    /path/to/File.hs:36:1: error:
    • Expected kind ‘Tree -> Constraint’,
        but ‘Tree a b c’ has kind ‘Constraint’
    |],
    "",
    [exon|
    /path/to/File.hs:36:1: error:
    • Could not deduce: Polysemy.Internal.Union.LocateEffect
                              (Resumable
                                e0
                                (Polysemy.AtomicState.AtomicState
                                    Foo.SomeType))
                              r
                            ~ '()
            arising from a use of ‘someFunc’
    |],
    "",
    [exon|
    /path/to/File.hs:36:1: error:
    • Could not deduce: Polysemy.Internal.Union.LocateEffect ((AtomicState SomeType) !! Error) r ~ '()
            arising from a use of ‘someFunc’
    |],
    "",
    [exon|
    /path/to/File.hs:36:1: error:
    • Unhandled effect 'Resumable e (AtomicState SomeType)'
    |],
    ""
    ]

target :: Vector Text
target = Vector.fromList [
  "/path/to/Module/File.hs \57505 13",
  "type mismatch",
  "TypeA",
  "TypeB",
  "",
  "/path/to/Module/File.hs \57505 19",
  "type not in scope",
  "Something",
  "",
  "/path/to/Module/File.hs \57505 2",
  "method not implemented: meth",
  "",
  "/path/to/File.hs \57505 5",
  "type mismatch",
  "t0 Data1 -> StateT (ReaderT e0) ()",
  "StateT (ReaderT (TVar Data)) a0",
  "",
  "/path/to/File.hs \57505 17",
  "redundant name imports",
  "ComposeSt, captureT, control, defaultLiftBaseWith, defaultRestoreM, embed, embed_",
  "Control.Monad.Trans.Control",
  "",
  "/path/to/File.hs \57505 24",
  "redundant module import",
  "Data.Either.Combinators",
  "",
  "/path/to/File.hs \57505 36",
  "syntax error",
  "",
  "/path/to/File.hs \57505 36",
  "!instance: func",
  "MonadIO (t m)",
  "",
  "/path/to/File.hs \57505 36",
  "variable not in scope",
  "var :: IO a0",
  "",
  "/path/to/File.hs \57505 36",
  "redundant name imports",
  "Name",
  "Module",
  "",
  "/path/to/File.hs \57505 36",
  "do-notation result discarded",
  "m ()",
  "",
  "/path/to/File.hs \57505 36",
  "invalid import name",
  "NoSuchName",
  "Mod.Ule",
  "",
  "/path/to/File.hs \57505 36",
  "module name mismatch",
  "Wrong.Module",
  "Correct.Module",
  "",
  "/path/to/File.hs \57505 36",
  "unknown module",
  "Non.Existing.Module",
  "",
  "/path/to/File.hs \57505 36",
  "!instance: constrainedFun",
  "Class t m",
  "",
  "/path/to/File.hs \57505 36",
  "type mismatch",
  "Found",
  "Req",
  "",
  "/path/to/File.hs \57505 36",
  "ambiguous type var for constraint",
  "a0",
  "funky",
  "Classy a0",
  "",
  "/path/to/File.hs \57505 36",
  "invalid qualified name",
  "Mod.name",
  "",
  "/path/to/File.hs \57505 36",
  "runtime error",
  "Prelude.undefined",
  "",
  "/path/to/File.hs \57505 37",
  "runtime error",
  "Prelude.undefined",
  "",
  "/path/to/File.hs \57505 999",
  "non-exhaustive patterns",
  "wrong",
  "",
  "/path/to/File.hs \57505 36",
  "!instance: \"to\"",
  "IsString a1",
  "",
  "/path/to/File.hs \57505 36",
  "type not in scope",
  "Something",
  "",
  "/path/to/File.hs \57505 36",
  "data constructor not in scope",
  "Dat",
  "",
  "/path/to/File.hs \57505 36",
  "non-exhaustive patterns",
  "nonex",
  "",
  "/path/to/File.hs \57505 36",
  "kind mismatch",
  "Constraint",
  "Tree -> Constraint",
  "",
  "/path/to/File.hs \57505 36",
  "!effect: AtomicState SomeType !! e0",
  "",
  "/path/to/File.hs \57505 36",
  "!effect: AtomicState SomeType !! Error",
  "",
  "/path/to/File.hs \57505 36",
  "!effect: AtomicState SomeType !! e",
  ""
  ]

parseHaskell ::
  Member (Error TestError) r =>
  Sem r ParsedOutput
parseHaskell =
    testError (haskellOutputParser haskellOutput)

test_parseHaskellErrors :: UnitTest
test_parseHaskellErrors =
  myoTest do
    ParsedOutput _ events <- parseHaskell
    target === ((.text) <$> (compileReport 1 events).lines)

parseHaskellGarbage ::
  Members [Test, Error TestError] r =>
  Sem r ParsedOutput
parseHaskellGarbage = do
  out <- Test.fixture [relfile|output/parse/haskell-garbage|]
  testError (haskellOutputParser (toText out))

garbageTarget :: Vector Text
garbageTarget =
  Vector.fromList [
    "/path/to/File.hs \57505 1",
    "variable not in scope",
    "var :: IO a0",
    "",
    "/path/to/File.hs \57505 1",
    "redundant name imports",
    "foo",
    "Mod",
    ""
    ]

test_parseGarbage :: UnitTest
test_parseGarbage =
  myoTest do
    ParsedOutput _ events <- parseHaskellGarbage
    garbageTarget === ((.text) <$> (compileReport 0 events).lines)
