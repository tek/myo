module Myo.AtomicState where

import Control.Lens (Lens', view, (.~))

atomicSet ::
  Member (AtomicState s) r =>
  Lens' s a ->
  a ->
  Sem r ()
atomicSet l a =
  atomicModify' (l .~ a)

atomicView ::
  Member (AtomicState s) r =>
  Lens' s a ->
  Sem r a
atomicView l =
  atomicGets (view l)
