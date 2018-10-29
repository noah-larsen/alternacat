package consoleApplication.commands

import utils.commands.IndexedCommand.IndexListCommand
import utils.commands.{Command, Commands, Parameter}

object ConnectSourceNodesSelectionCommands extends Commands {

  override type CommandType = ConnectSourceNodesSelectionCommand
  sealed abstract class ConnectSourceNodesSelectionCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object SelectNodes extends ConnectSourceNodesSelectionCommand with IndexListCommand
  object SelectAll extends ConnectSourceNodesSelectionCommand(specifiedLetterName = Some('a'))
  object Back extends ConnectSourceNodesSelectionCommand

  override protected def help: (ConnectSourceNodesSelectionCommand) => String = {
    case SelectNodes => ""
    case SelectAll => ""
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodesSelectionCommands.type], classOf[ConnectSourceNodesSelectionCommand])

}
