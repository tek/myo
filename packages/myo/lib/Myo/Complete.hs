module Myo.Complete where

import Chiasma.Data.Ident (Ident (Str))
import qualified Data.Text as Text
import Ribosome (Report, resumeReport)

import Myo.Command.Data.Command (Command (Command), displayName, ident)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Data.CommandName (CommandName (CommandName))
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

myoCompleteCommand ::
  Members [Commands !! CommandError, Stop Report] r =>
  Text ->
  Text ->
  Int ->
  Sem r [Text]
myoCompleteCommand lead _ _ = do
  cmds <- resumeReport Commands.all
  pure (mapMaybe match cmds)
  where
    match Command {ident = CommandId (Str ident)} | isPrefix ident =
      Just ident
    match Command {displayName = Just (CommandName name)} | isPrefix name =
      Just name
    match _ =
      Nothing
    isPrefix =
      Text.isPrefixOf lead
