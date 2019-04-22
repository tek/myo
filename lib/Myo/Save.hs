module Myo.Save where

import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Log (pushCommandLogs)
import Myo.Settings (resetOnSave)

myoSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  m ()
myoSave = do
  push <- setting resetOnSave
  when push pushCommandLogs
