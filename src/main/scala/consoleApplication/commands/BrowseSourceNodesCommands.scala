package consoleApplication.commands

import utils.commands.IndexedCommand.IndexCommand
import utils.commands.{Command, Commands, Parameter}

object BrowseSourceNodesCommands extends Commands {

  override type CommandType = BrowseSourceNodesCommand
  sealed abstract class BrowseSourceNodesCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends BrowseSourceNodesCommand with IndexCommand
  object GoUp extends BrowseSourceNodesCommand(specifiedLetterName = Some('u'))
  object RelatedNodes extends BrowseSourceNodesCommand
  object EditRelatedNodes extends BrowseSourceNodesCommand
  object BackToMainMenu extends BrowseSourceNodesCommand

  override protected def help: (BrowseSourceNodesCommand) => String = {
    case GoTo =>
      "Moves to the source node identified by the corresponding # in the displayed table. The table contains the children of any node currently in view, and the forest " +
        "roots otherwise."
    case GoUp => "Moves to the parent of this source node."
    case RelatedNodes => "Displays the nodes from the target forest that are related to this source node."
    case EditRelatedNodes => "Opens a menu containing options for editing which nodes from the target forest are related to this source node."
    case BackToMainMenu => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[BrowseSourceNodesCommands.type], classOf[BrowseSourceNodesCommand])

}
