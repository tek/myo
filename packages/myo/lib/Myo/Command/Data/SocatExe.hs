module Myo.Command.Data.SocatExe where

import Path (Abs, File, Path)

newtype SocatExe =
  SocatExe { unSocatExe :: Path Abs File }
  deriving stock (Eq, Show)
