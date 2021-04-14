module Myo.Save where

import Prelude hiding (state)
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
      logDebug @Text "pushing command log on save" *>
      runExceptT @CommandError pushCommandLogs *>
      updateLastSave

myoSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  m ()
myoSave =
  (`when` modifyM sensibleSave) =<< setting Settings.resetOnSave

saveAll ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepError e SettingError m =>
  m ()
saveAll = do
  myoSave
  vimCommand "noautocmd silent! wall"

preCommandSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m ()
preCommandSave =
  (`when` saveAll) =<< setting Settings.saveBeforeRun
