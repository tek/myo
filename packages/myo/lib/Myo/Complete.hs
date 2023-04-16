module Myo.Complete where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Text as Text

import Myo.Command.Data.Command (Command (Command), displayName, ident)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.CommandId (CommandId (CommandId))

myoCompleteCommand ::
  Member (AtomicState CommandState) r =>
  Text ->
  Text ->
  Int ->
  Sem r [Text]
myoCompleteCommand lead _ _ = do
  cmds <- atomicGets (.commands)
  pure (mapMaybe match cmds)
  where
    match Command { ident = CommandId (Str ident) } | isPrefix ident =
      Just ident
    match Command { displayName = Just name } | isPrefix name =
      Just name
    match _ =
      Nothing
    isPrefix =
      Text.isPrefixOf lead
