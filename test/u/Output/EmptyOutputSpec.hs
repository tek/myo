{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.EmptyOutputSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.DeepError (catchAt, throwHoist)
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Ribosome.Nvim.Api.IO (nvimListWins)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Test.Framework

import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')

emptyOutputSpec :: MyoN ()
emptyOutputSpec = do
  initialize''
  catchAt catchNoEvents $ renderParseResult (Ident.Str "test") []
  wins <- nvimListWins
  gassertEqual 1 (length wins)
  where
    catchNoEvents (NoEvents _) = return ()
    catchNoEvents e = throwHoist e

test_emptyOutput :: IO ()
test_emptyOutput =
  tmuxGuiSpecDef emptyOutputSpec
