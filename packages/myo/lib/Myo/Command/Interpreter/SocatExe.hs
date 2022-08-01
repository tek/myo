module Myo.Command.Interpreter.SocatExe where

import Path (relfile)
import Process (resolveExecutable)
import Ribosome (LogReport)
import Ribosome.Data.CustomConfig (CustomConfig (CustomConfig))
import qualified Ribosome.Report as Report

import Myo.Command.Data.SocatExe (SocatExe (SocatExe, unSocatExe))
import Myo.Data.CliOptions (CliOptions (CliOptions))

interpretReaderSocatExe ::
  Members [Reader (CustomConfig CliOptions), DataLog LogReport, Embed IO] r =>
  InterpreterFor (Reader (Maybe SocatExe)) r
interpretReaderSocatExe sem = do
  CustomConfig (CliOptions cliSocat) <- ask
  socat <- rightToMaybe <$> resolveExecutable [relfile|socat|] (unSocatExe <$> cliSocat)
  unless (isJust socat) do
    Report.warn err [err]
  runReader (SocatExe <$> socat) sem
  where
    err =
      "Can't find `socat`. Processing tmux output will not work."
