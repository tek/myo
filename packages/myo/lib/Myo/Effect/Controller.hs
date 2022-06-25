module Myo.Effect.Controller where

import Chiasma.Data.Ident (Ident)

import Myo.Command.Data.Command (Command)

data Controller :: Effect where
  RunIdent :: Ident -> Controller m ()
  RunCommand :: Command -> Controller m ()

makeSem ''Controller
