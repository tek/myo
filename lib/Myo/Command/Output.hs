module Myo.Command.Output(
  renderParseResult,
) where

import Myo.Output.Data.ParsedOutput (ParsedOutput)

renderParseResult :: Monad m => [ParsedOutput] -> m ()
renderParseResult _ =
  return ()
