module Myo.Interpreter.InputIdent where

import Chiasma.Data.Ident (Ident (Uuid))
import Conc (interpretAtomic)
import System.Random (Random (random), newStdGen)

interpretInputIdentRandom ::
  Member (Embed IO) r =>
  InterpreterFor (Input Ident) r
interpretInputIdentRandom sem = do
  q <- embed newStdGen
  interpretAtomic q $ runInputSem (Uuid <$> atomicState' (swap . random)) (raiseUnder sem)
