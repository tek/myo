module Myo.Save where

import Conc (Lock, lockOrSkip_)
import qualified Log
import Polysemy.Chronos (ChronosTime)
import Ribosome (Handler, Rpc, Settings, noautocmd, resumeHandlerError, silentBang)
import Ribosome.Api (nvimCommand)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Settings as Settings
import qualified Time

import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Data.LastSave (LastSave (LastSave))
import Myo.Data.SaveLock (SaveLock)
import qualified Myo.Settings as Settings (resetOnSave, saveBeforeRun, saveInterval)

checkInterval ::
  Members [AtomicState LastSave, Settings, ChronosTime] r =>
  Sem r Bool
checkInterval = do
  now <- Time.now
  interval <- Settings.get Settings.saveInterval
  atomicState' \ (LastSave prev) ->
    if Time.diff now prev >= interval
    then (LastSave now, True)
    else (LastSave prev, False)

sensibleSave ::
  Members [AtomicState LastSave, Lock @@ SaveLock, CommandLog, Settings, ChronosTime, Log, Resource] r =>
  Sem r ()
sensibleSave =
  tag $ lockOrSkip_ $ whenM checkInterval do
    Log.debug "Archiving command logs on save"
    CommandLog.archiveAll

save ::
  Members [AtomicState LastSave, Lock @@ SaveLock, CommandLog, Settings, ChronosTime, Log, Resource] r =>
  Sem r ()
save =
  whenM (Settings.get Settings.resetOnSave) sensibleSave

myoSave ::
  Members [AtomicState LastSave, Lock @@ SaveLock, CommandLog, Settings !! SettingError, ChronosTime, Log, Resource] r =>
  Handler r ()
myoSave =
  resumeHandlerError save

saveAll ::
  Members [AtomicState LastSave, Lock @@ SaveLock, CommandLog, Settings, Rpc, ChronosTime, Log, Resource] r =>
  Sem r ()
saveAll = do
  save
  noautocmd $ silentBang do
    nvimCommand "wall"

preCommandSave ::
  Members [AtomicState LastSave, Lock @@ SaveLock, CommandLog, Settings, Rpc, ChronosTime, Log, Resource] r =>
  Sem r ()
preCommandSave =
  whenM (Settings.get Settings.saveBeforeRun) saveAll
