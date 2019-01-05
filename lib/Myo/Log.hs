module Myo.Log(
  debug,
  info,
  debugS,
  infoS,
  R.p,
) where

import Ribosome.Control.Ribo (Ribo)
import qualified Ribosome.Log as R (debug, info, p)

debug :: String -> Ribo e ()
debug = R.debug "myo"

info :: String -> Ribo e ()
info = R.info "myo"

debugS :: Show a => a -> Ribo e ()
debugS = R.debug "myo"

infoS :: Show a => a -> Ribo e ()
infoS = R.info "myo"
