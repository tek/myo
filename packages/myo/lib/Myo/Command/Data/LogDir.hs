module Myo.Command.Data.LogDir where

import Path (Abs, Dir, Path)

newtype LogDir =
  LogDir { unLogDir :: Path Abs Dir }
  deriving stock (Eq, Show)
