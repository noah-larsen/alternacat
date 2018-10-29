package consoleApplication.commands

import consoleApplication.CommonParameters.PartOfName
import utils.commands.IndexedCommand.IndexCommand
import utils.commands.Parameter._
import utils.commands.{Command, Commands, Parameter}

import scala.util.Try

object GoToTargetNodesCommands extends Commands {

  override type CommandType = GoToTargetNodesCommand
  sealed abstract class GoToTargetNodesCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object GoTo extends GoToTargetNodesCommand with IndexCommand
  object GoUp extends GoToTargetNodesCommand(specifiedLetterName = Some('u'))
  object SetAsRelated extends GoToTargetNodesCommand
  object RemoveRelatedness extends GoToTargetNodesCommand
  object CreateNewRelatedTargetRootNode extends GoToTargetNodesCommand(Seq(Unrelated, PartOfName))
  object CreateNewRelatedTargetChildNode extends GoToTargetNodesCommand(Seq(Unrelated, PartOfName))
  object CreateNewTargetChildNode extends GoToTargetNodesCommand(Seq(PartOfName))
  object AbbreviationsForNamingTargetNodes extends GoToTargetNodesCommand
  object EditName extends GoToTargetNodesCommand
  object MoveChildWithinAnotherChildOrUp extends GoToTargetNodesCommand(Seq(NumberOfChildToMove, NumberOfChildToMoveInto))
  object MoveRootWithinAnotherRoot extends GoToTargetNodesCommand(Seq(NumberOfRootToMove, NumberOfRootToMoveInto))
  object Delete extends GoToTargetNodesCommand
  object Back extends GoToTargetNodesCommand


  object Unrelated extends OptionParameter
  object NumberOfChildToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfChildToMoveInto extends OptionalParameter(x => Try(x.toInt))
  object NumberOfRootToMove extends ValueParameter(x => Try(x.toInt))
  object NumberOfRootToMoveInto extends ValueParameter(x => Try(x.toInt))


  override protected def help: (GoToTargetNodesCommand) => String = {
    case GoTo => ""
    case GoUp => ""
    case SetAsRelated => ""
    case RemoveRelatedness => ""
    case CreateNewRelatedTargetRootNode => ""
    case CreateNewRelatedTargetChildNode => ""
    case CreateNewTargetChildNode => ""
    case AbbreviationsForNamingTargetNodes => ""
    case EditName => ""
    case MoveChildWithinAnotherChildOrUp => ""
    case MoveRootWithinAnotherRoot => ""
    case Delete => ""
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[GoToTargetNodesCommands.type], classOf[GoToTargetNodesCommand])

}
