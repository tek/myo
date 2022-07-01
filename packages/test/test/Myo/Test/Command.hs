module Myo.Test.Command where

import Polysemy.Test (TestError)
import Ribosome (Handler)
import Ribosome.Test (testHandlerAsync)

withTestHandlerAsync ::
  Members [Error TestError, Async] r =>
  Handler r a ->
  Sem r () ->
  Sem r a
withTestHandlerAsync handler test = do
  thread <- testHandlerAsync do
    handler
  test
  thread
