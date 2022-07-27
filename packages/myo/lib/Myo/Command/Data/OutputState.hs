module Myo.Command.Data.OutputState where

import Ribosome.Syntax (Syntax)

import Myo.Data.CommandId (CommandId)
import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.OutputEvents (OutputEvents)
import Myo.Output.Data.ParseReport (ParseReport)

data OutputState =
  OutputState {
    command :: CommandId,
    syntax :: [Syntax],
    events :: OutputEvents,
    currentEvent :: EventIndex.Absolute,
    report :: Maybe ParseReport
  }
  deriving stock (Eq, Show, Generic)
