{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Env(
  Env(..),
  _command,
  _errors,
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Ribosome.Data.Errors (Errors)
import Myo.Command.Data.CommandState (CommandState)

data Env =
  Env {
    command :: CommandState,
    errors :: Errors
  }

makeClassy_ ''Env

instance Default Env where
  def = Env def def
