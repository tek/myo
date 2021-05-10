module Myo.Output.Data.ParsedOutput where

import Control.Lens (allOf, each)
import Ribosome.Data.Syntax (Syntax)

import Myo.Output.Data.OutputEvents (OutputEvents)
import qualified Myo.Output.Data.OutputEvents as OutputEvents (events)

data ParsedOutput =
  ParsedOutput {
    _syntax :: Syntax,
    _events :: OutputEvents
  }
  deriving (Eq, Show)

makeClassy ''ParsedOutput

allEmpty :: [ParsedOutput] -> Bool
allEmpty =
  allOf (each . events . OutputEvents.events) null
