module Myo.Output.Data.ParsedOutput where

import Control.Lens (allOf)
import Data.Generics.Labels ()
import Ribosome.Data.Syntax (Syntax)

import Myo.Output.Data.OutputEvents (OutputEvents)

data ParsedOutput =
  ParsedOutput {
    syntax :: Syntax,
    events :: OutputEvents
  }
  deriving stock (Eq, Show, Generic)

empty :: ParsedOutput -> Bool
empty =
  allOf (#events . #events) null

allEmpty :: [ParsedOutput] -> Bool
allEmpty =
  allOf (each . #events . #events) null
