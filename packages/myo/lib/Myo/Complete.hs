module Myo.Complete where

import qualified Chiasma.Data.Ident as Ident
import qualified Data.Text as Text

import Myo.Command.Data.Command (Command (Command))
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)

myoCompleteCommand ::
  Member (AtomicState CommandState) r =>
  Text ->
  Sem r [Text]
myoCompleteCommand prefix = do
  cmds <- atomicGets CommandState.commands
  pure (catMaybes (match <$> cmds))
  where
    match (Command _ (Ident.Str ident) _ _ _ _ _ _ _) | isPrefix ident =
      Just ident
    match (Command _ _ _ _ _ (Just name) _ _ _) | isPrefix name =
      Just name
    match _ =
      Nothing
    isPrefix =
      Text.isPrefixOf prefix
