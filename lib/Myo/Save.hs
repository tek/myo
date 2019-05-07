module Myo.Save where

import qualified Control.Lens as Lens (set)
import Control.Monad.DeepState (modifyM)
import Data.Hourglass (Elapsed(Elapsed), Seconds(Seconds))
import Ribosome.Config.Setting (setting)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (vimCommand)
import Time.System (timeCurrent)

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Log (pushCommandLogs)
import Myo.Data.Env (Env(Env, _lastSave))
import qualified Myo.Data.Env as Env (lastSave)
import qualified Myo.Settings as Settings (resetOnSave, saveBeforeRun)

updateLastSave ::
  MonadIO m =>
  MonadDeepState s Env m =>
  m ()
updateLastSave =
  setL @Env Env.lastSave =<< liftIO timeCurrent

sensibleSave ::
  MonadIO m =>
  Env ->
  m Env
sensibleSave state@Env{ _lastSave = last', .. } = do
  now <- liftIO timeCurrent
  execStateT (when (shouldSave now) save) state
  where
    shouldSave now =
      last' - now > Elapsed (Seconds 1)
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
  vimCommand "wall"

preCommandSave ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s Env m =>
  m ()
preCommandSave =
  (`when` saveAll) =<< setting Settings.saveBeforeRun
