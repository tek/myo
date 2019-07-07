module Myo.Output.Data.ParsedOutput where

import Control.Lens (allOf, each)
import Ribosome.Data.Syntax (Syntax)

import qualified Myo.Output.Data.EventIndex as EventIndex (Relative)
import Myo.Output.Data.ParseReport (ParseReport)
import qualified Myo.Output.Data.ParseReport as ParseReport (events)

data ParsedOutput =
  ParsedOutput {
    _syntax :: Syntax,
    _report :: ParseReport EventIndex.Relative
  }
  deriving (Eq, Show)

makeClassy ''ParsedOutput

allEmpty :: [ParsedOutput] -> Bool
allEmpty =
  allOf (each . report . ParseReport.events) null
