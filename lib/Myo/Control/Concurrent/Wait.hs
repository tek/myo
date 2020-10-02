module Myo.Control.Concurrent.Wait where

-- |Specifies the maximum number of retries and the interval in seconds for 'waitIO'.
data Retry =
  Retry Int Double
  deriving Show

instance Default Retry where
  def = Retry 30 0.1

-- |Error description for 'waitIO'
data WaitError =
  NotStarted
  |
  ConditionUnmet Text
  deriving (Eq, Show)

-- |Execute an IO thunk repeatedly until either the supplied condition produces a 'Right' or the maximum number of
-- retries specified in the `Retry` parameter has been reached.
-- Returns the value produced by the condition.
waitIO ::
  MonadIO m =>
  Retry ->
  m a ->
  (a -> m (Either Text b)) ->
  m (Either WaitError b)
waitIO (Retry maxRetry interval) thunk cond =
  wait maxRetry (Left NotStarted)
  where
    wait 0 reason = return reason
    wait count _ = do
      ea <- thunk
      result <- check ea
      case result of
        Right a ->
          return $ Right a
        Left reason ->
          recurse reason count
    recurse reason count = do
      sleep interval
      wait (count - 1) (Left reason)
    check a =
      cond a <&> \case
        Right b -> Right b
        Left reason -> Left (ConditionUnmet reason)

-- |Calls 'waitIO' with the default configuration of 30 retries every 100ms.
waitIODef ::
  MonadIO m =>
  m a ->
  (a -> m (Either Text b)) ->
  m (Either WaitError b)
waitIODef =
  waitIO def

-- |Same as 'waitIO', but the condition returns 'Bool' and the result is the result of the thunk.
waitIOPred ::
  MonadIO m =>
  Retry ->
  m a ->
  (a -> m Bool) ->
  m (Either WaitError a)
waitIOPred retry thunk pred' =
  waitIO retry thunk cond
  where
    cond a = pred' a <&> \case
      True -> Right a
      False -> Left "predicate returned False"

-- |Calls 'waitIOPred' with the default configuration of 30 retries every 100ms.
waitIOPredDef ::
  MonadIO m =>
  m a ->
  (a -> m Bool) ->
  m (Either WaitError a)
waitIOPredDef =
  waitIOPred def
