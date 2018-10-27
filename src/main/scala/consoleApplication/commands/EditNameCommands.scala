package consoleApplication.commands

import consoleApplication.CommonParameters.PartOfName
import utils.commands.{Command, Commands, Parameter}

object EditNameCommands extends Commands {

  override type CommandType = EditNameCommand
  sealed abstract class EditNameCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object Edit extends EditNameCommand(Seq(PartOfName))
  object AbbreviationsForNamingTargetNodes extends EditNameCommand
  object Back extends EditNameCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[EditNameCommands.type], classOf[EditNameCommand])

}
