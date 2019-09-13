{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.EmptyOutputSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.DeepError (catchAt, throwHoist)
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Ribosome.Nvim.Api.IO (nvimListWins)
import Ribosome.Test.Tmux (tmuxSpecDef)
import Test.Framework

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')

emptyOutputSpec :: Myo ()
emptyOutputSpec = do
  initialize''
  storeParseResult (Ident.Str "test") def
  catchAt catchNoEvents compileAndRenderReport
  wins <- nvimListWins
  gassertEqual 1 (length wins)
  where
    catchNoEvents (NoEvents _) = return ()
    catchNoEvents e = throwHoist e

test_emptyOutput :: IO ()
test_emptyOutput =
  tmuxSpecDef emptyOutputSpec
