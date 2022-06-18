module Myo.Test.Output.EmptyOutputTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Hedgehog ((===))
import Ribosome.Nvim.Api.IO (nvimListWins)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Tmux (tmuxTestDef)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import Myo.Init (initialize'')
import Myo.Output.Data.OutputError (OutputError(NoEvents))
import Myo.Test.Unit (MyoTest)

emptyOutputTest :: MyoTest ()
emptyOutputTest = do
  lift initialize''
  storeParseResult (Ident.Str "test") def
  catchAt catchNoEvents compileAndRenderReport
  wins <- nvimListWins
  1 === length wins
  where
    catchNoEvents (NoEvents _) = pure ()
    catchNoEvents e = stop e

test_emptyOutput :: UnitTest
test_emptyOutput =
  tmuxTestDef emptyOutputTest
