package consoleApplication.commands

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import utils.commands.{Command, Commands, Parameter}

object EditRelatedNodesCommands extends Commands {

  override type CommandType = EditRelatedNodeCommand
  sealed abstract class EditRelatedNodeCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object RelatedNodes extends EditRelatedNodeCommand
  object Descendants extends EditRelatedNodeCommand(Seq(MaxDepth))
  object GoToTargetNodes extends EditRelatedNodeCommand(specifiedLetterName = Some('t'))
  object SearchTargetNodes extends EditRelatedNodeCommand(Seq(Keyword))
  object Back extends EditRelatedNodeCommand

  override protected def help: (EditRelatedNodeCommand) => String = {
    case RelatedNodes => ConnectSourceNodeCommands.relatedTargetNodesDescription
    case Descendants => ConnectSourceNodeCommands.descendantsHelpDescription
    case GoToTargetNodes => ConnectSourceNodeCommands.goToTargetNodesDescription
    case SearchTargetNodes => ConnectSourceNodeCommands.searchTargetNodesDescription
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[EditRelatedNodesCommands.type], classOf[EditRelatedNodeCommand])

}
