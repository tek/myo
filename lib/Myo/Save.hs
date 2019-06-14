module Myo.Save where

import Control.Monad.DeepState (modifyM)
import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (vimCommand)
import Ribosome.System.Time (secondsP)
import Time.System (timeCurrentP)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Log (pushCommandLogs)
import Myo.Data.Env (Env(Env, _lastSave))
import qualified Myo.Data.Env as Env (lastSave)
import qualified Myo.Settings as Settings (resetOnSave, saveBeforeRun, saveInterval)

updateLastSave ::
  MonadIO m =>
  MonadDeepState s Env m =>
  m ()
updateLastSave =
  setL @Env Env.lastSave =<< liftIO timeCurrentP

sensibleSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  Env ->
  m Env
sensibleSave state@Env{ _lastSave = last', .. } = do
  saveTimeout <- secondsP <$> setting Settings.saveInterval
  now <- liftIO timeCurrentP
  execStateT (when (shouldSave now saveTimeout) save) state
  where
    shouldSave now saveTimeout =
      now - last' > saveTimeout
    save =
      runExceptT @CommandError pushCommandLogs *> updateLastSave

myoSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m ()
myoSave =
  (`when` modifyM sensibleSave) =<< setting Settings.resetOnSave

saveAll ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s Env m =>
  m ()
saveAll = do
  updateLastSave
  vimCommand "noautocmd wall"

preCommandSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m ()
preCommandSave =
  (`when` saveAll) =<< setting Settings.saveBeforeRun
