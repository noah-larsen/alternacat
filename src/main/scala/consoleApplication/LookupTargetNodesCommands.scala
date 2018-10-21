package consoleApplication

import consoleApplication.CommonParameters.PartOfName
import utils.commands.IndexedCommand.IndexCommand
import utils.commands.Parameter._
import utils.commands.{Command, Commands, Parameter}

import scala.util.Try

object LookupTargetNodesCommands extends Commands {

  override type CommandType = LookupTargetNodesCommand
  sealed abstract class LookupTargetNodesCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends LookupTargetNodesCommand with IndexCommand
  object GoUp extends LookupTargetNodesCommand(specifiedLetterName = Some('u'))
  object SetAsRelated extends LookupTargetNodesCommand
  object RemoveRelatedness extends LookupTargetNodesCommand
  object CreateNewTargetRootNode extends LookupTargetNodesCommand(Seq(Unrelated, PartOfName))
  object CreateNewTargetChildNode extends LookupTargetNodesCommand(Seq(Unrelated, PartOfName))
  object AbbreviationsForNamingTargetNodes extends LookupTargetNodesCommand
  object EditName extends LookupTargetNodesCommand
  object MoveChildWithinAnotherChildOrUp extends LookupTargetNodesCommand(Seq(NumberOfChildToMove, NumberOfChildToMoveInto))
  object MoveRootWithinAnotherRoot extends LookupTargetNodesCommand(Seq(NumberOfRootToMove, NumberOfRootToMoveInto))
  object Delete extends LookupTargetNodesCommand
  object Back extends LookupTargetNodesCommand


  object Unrelated extends OptionParameter
  object NumberOfChildToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfChildToMoveInto extends OptionalParameter(x => Try(x.toInt))
  object NumberOfRootToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfRootToMoveInto extends ValueParameter(x => Try(x.toInt))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[LookupTargetNodesCommands.type], classOf[LookupTargetNodesCommand])

}
