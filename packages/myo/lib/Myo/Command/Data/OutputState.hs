module Myo.Command.Data.OutputState where

import Chiasma.Data.Ident (Ident)
import Ribosome.Syntax (Syntax)

import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.OutputEvents (OutputEvents)
import Myo.Output.Data.ParseReport (ParseReport)

data OutputState =
  OutputState {
    command :: Ident,
    syntax :: [Syntax],
    events :: OutputEvents,
    currentEvent :: EventIndex.Absolute,
    report :: Maybe ParseReport
  }
  deriving stock (Eq, Show, Generic)
