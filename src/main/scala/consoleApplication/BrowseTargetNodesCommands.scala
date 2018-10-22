package consoleApplication

import consoleApplication.CommonParameters.PartOfName
import utils.commands.IndexedCommand.IndexCommand
import utils.commands.Parameter._
import utils.commands.{Command, Commands, Parameter}

import scala.util.Try

object BrowseTargetNodesCommands extends Commands {

  override type CommandType = BrowseTargetNodesCommand
  sealed abstract class BrowseTargetNodesCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends BrowseTargetNodesCommand with IndexCommand
  object GoUp extends BrowseTargetNodesCommand(specifiedLetterName = Some('u'))
  object SetAsRelated extends BrowseTargetNodesCommand
  object RemoveRelatedness extends BrowseTargetNodesCommand
  object CreateNewRelatedTargetRootNode extends BrowseTargetNodesCommand(Seq(Unrelated, PartOfName))
  object CreateNewRelatedTargetChildNode extends BrowseTargetNodesCommand(Seq(Unrelated, PartOfName))
  object CreateNewTargetChildNode extends BrowseTargetNodesCommand(Seq(PartOfName))
  object AbbreviationsForNamingTargetNodes extends BrowseTargetNodesCommand
  object EditName extends BrowseTargetNodesCommand
  object MoveChildWithinAnotherChildOrUp extends BrowseTargetNodesCommand(Seq(NumberOfChildToMove, NumberOfChildToMoveInto))
  object MoveRootWithinAnotherRoot extends BrowseTargetNodesCommand(Seq(NumberOfRootToMove, NumberOfRootToMoveInto))
  object Delete extends BrowseTargetNodesCommand
  object Back extends BrowseTargetNodesCommand


  object Unrelated extends OptionParameter
  object NumberOfChildToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfChildToMoveInto extends OptionalParameter(x => Try(x.toInt))
  object NumberOfRootToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfRootToMoveInto extends ValueParameter(x => Try(x.toInt))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[BrowseTargetNodesCommands.type], classOf[BrowseTargetNodesCommand])

}
