{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Env(
  Env(..),
  _command,
  _ui,
  _errors,
) where

import Control.Lens (makeClassy_)
import Data.Default.Class (Default(def))
import Ribosome.Data.Errors (Errors)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Ui.Data.UiState (UiState)

data Env =
  Env {
    command :: CommandState,
    ui :: UiState,
    errors :: Errors
  }
  deriving (Show, Eq)

makeClassy_ ''Env

instance Default Env where
  def = Env def def def
