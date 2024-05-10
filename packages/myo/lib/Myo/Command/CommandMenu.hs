module Myo.Command.CommandMenu where

import Chiasma.Data.Ident (Ident)
import Exon (exon)
import Ribosome (Handler, ReportLog, Rpc, RpcError, ScratchId (ScratchId), mapReports, resumeReports, scratch)
import Ribosome.Menu (
  MenuItem,
  MenuResult (Error, Success),
  MenuWidget,
  ModalState,
  ModalWindowMenus,
  WindowMenu,
  simpleMenuItem,
  staticWindowMenu,
  withFocus,
  withInsert,
  )

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command, ident)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSpec (renderCommandSpec)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Edit (EditItem, editCommand)
import Myo.Command.Run (runIdentAsync)
import Myo.Data.CommandId (CommandId)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import Myo.Effect.History (History)

data CommandAction =
  Run CommandId
  |
  Edit CommandId
  deriving stock (Eq, Show, Generic)

cmdlineItem :: Maybe CommandId -> Command -> Maybe [Text] -> MenuItem CommandId
cmdlineItem idOverride command compiled =
  simpleMenuItem ident menuItemText
  where
    ident = fromMaybe command.ident idOverride

    menuItemText = [exon|[#{Command.describe command}] #{cmdlineDisplay}|]

    cmdlineDisplay = fromMaybe "<no command line>" (head cmdline)

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

run :: MenuWidget (ModalState CommandId) r CommandAction
run = withFocus \ i -> pure (Run i)

edit :: MenuWidget (ModalState CommandId) r CommandAction
edit = withFocus \ i -> pure (Edit i)

commandMenu ::
  Members (CommandMenuStack ui) r =>
  Members [Stop CommandError, Stop RpcError] r =>
  Sem r (MenuResult CommandAction)
commandMenu = do
  items <- commandItems
  staticWindowMenu items def opts actions
  where
    opts =
      def & #items .~ (scratch (ScratchId name) & #filetype ?~ name)
    name =
      "myo-commands"

    actions =
      [
        (withInsert "<cr>", withFocus (pure . Run)),
        ("e", edit)
      ]

myoCommands ::
  Members (CommandMenuStack WindowMenu) r =>
  Members [ModalWindowMenus EditItem !! RpcError, ReportLog, History !! RunError] r =>
  Members [Controller !! RunError, Input Ident, Async] r =>
  Handler r ()
myoCommands =
  resumeReports @[Controller, Rpc] @[_, _] $ mapReports @[RpcError, CommandError, RunError] do
    commandMenu >>= \case
      Success (Run ident) ->
        runIdentAsync ident mempty
      Success (Edit ident) ->
        editCommand ident
      Error err ->
        stop (CommandError.Misc err)
      _ ->
        unit
