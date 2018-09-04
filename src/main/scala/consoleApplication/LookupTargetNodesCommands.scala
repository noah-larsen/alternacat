package consoleApplication

import utils.commands.IndexedCommand.IndexCommand
import utils.commands.Parameter.{ListParameter, StringsParameter, ValueParameter}
import utils.commands.{Command, Commands, Parameter}

object LookupTargetNodesCommands extends Commands {

  override type CommandType = LookupTargetNodesCommand
  sealed abstract class LookupTargetNodesCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends LookupTargetNodesCommand with IndexCommand
  object GoUp extends LookupTargetNodesCommand(specifiedLetterName = Some('u'))
  object MarkRelated extends LookupTargetNodesCommand
  object RemoveRelatedness extends LookupTargetNodesCommand
  object CreateNewTargetNodeHere extends LookupTargetNodesCommand(Seq(PartOfName))
  object Back extends LookupTargetNodesCommand


  object PartOfName extends StringsParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[LookupTargetNodesCommands.type], classOf[LookupTargetNodesCommand])

}
