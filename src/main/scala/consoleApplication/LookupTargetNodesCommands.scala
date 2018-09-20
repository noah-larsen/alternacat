package consoleApplication

import consoleApplication.CommonParameters.PartOfName
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
  object CreateNewTargetRootNode extends LookupTargetNodesCommand(Seq(PartOfName))
  object CreateNewTargetChildNode extends LookupTargetNodesCommand(Seq(PartOfName))
  object AbbreviationsForNamingTargetNodes extends LookupTargetNodesCommand
  object EditName extends LookupTargetNodesCommand
  object Delete extends LookupTargetNodesCommand
  object Back extends LookupTargetNodesCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[LookupTargetNodesCommands.type], classOf[LookupTargetNodesCommand])

}
