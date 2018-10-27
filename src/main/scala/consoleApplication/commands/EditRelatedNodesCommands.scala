package consoleApplication.commands

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import utils.commands.{Command, Commands, Parameter}

object EditRelatedNodesCommands extends Commands {

  override type CommandType = EditRelatedNodeCommand
  sealed abstract class EditRelatedNodeCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object RelatedNodes extends EditRelatedNodeCommand
  object Descendants extends EditRelatedNodeCommand(Seq(MaxDepth))
  object LookupTargetNodes extends EditRelatedNodeCommand
  object SearchTargetNodes extends EditRelatedNodeCommand(Seq(Keyword))
  object Back extends EditRelatedNodeCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[EditRelatedNodesCommands.type], classOf[EditRelatedNodeCommand])

}
