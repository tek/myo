module Myo.Effect.Outputs where

import Myo.Command.Data.OutputState (OutputState)
import qualified Myo.Output.Data.EventIndex as EventIndex
import Myo.Output.Data.ParseReport (ParseReport)

data Outputs :: Effect where
  CurrentOutput :: Outputs m (Maybe OutputState)
  SetCurrentOutput :: OutputState -> Outputs m ()
  SetCurrentReport :: ParseReport -> Outputs m ()
  SetCurrentEvent :: EventIndex.Absolute -> Outputs m ()

makeSem ''Outputs
