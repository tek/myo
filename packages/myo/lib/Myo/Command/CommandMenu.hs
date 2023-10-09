module Myo.Command.CommandMenu where

import qualified Data.Text as Text
import Exon (exon)
import Ribosome (Handler, ReportLog, Rpc, RpcError, ScratchId (ScratchId), mapReports, resumeReports, scratch)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Error, Success),
  ModalWindowMenus,
  WindowMenu,
  simpleMenuItem,
  staticWindowMenu,
  withFocus,
  )

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command, ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSpec (renderCommandSpec)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Run (runIdentAsync)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)

cmdlineItem :: Maybe CommandId -> Command -> Maybe [Text] -> MenuItem CommandId
cmdlineItem idOverride command compiled =
  simpleMenuItem ident menuItemText
  where
    ident = fromMaybe command.ident idOverride

    menuItemText = [exon|[#{Command.describe command}] #{cmdlineDisplay}|]

    cmdlineDisplay = Text.take 100 (fromMaybe "<no command line>" (head cmdline))

    cmdline = fromMaybe (renderCommandSpec command.cmdLines) compiled

commandItem :: Command -> MenuItem CommandId
commandItem command = cmdlineItem Nothing command Nothing

commandItems ::
  Members [Commands !! CommandError, Stop CommandError] r =>
  Sem r [MenuItem CommandId]
commandItems = do
  cmds <- stopNote CommandError.NoCommands . nonEmpty =<< restop Commands.all
  pure (toList (commandItem <$> cmds))

type CommandMenuStack ui =
  [
    ModalWindowMenus CommandId !! RpcError,
    Commands !! CommandError,
    Rpc !! RpcError,
    Log
  ]

commandMenu ::
  Members (CommandMenuStack ui) r =>
  Members [Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult CommandId)
commandMenu = do
  items <- commandItems
  staticWindowMenu items def opts [("<cr>", withFocus pure)]
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
    name =
      "myo-commands"

myoCommands ::
  Members (CommandMenuStack WindowMenu) r =>
  Members [Controller !! RunError, ReportLog, Async] r =>
  Handler r ()
myoCommands =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError] do
    commandMenu >>= \case
      Success ident ->
        runIdentAsync ident mempty
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
