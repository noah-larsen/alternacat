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


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[BrowseSourceNodesCommands.type], classOf[BrowseSourceNodesCommand])

}
