package consoleApplication.commands

import utils.commands.{Command, Commands}

object YesNoCommands extends Commands {

  override type CommandType = YesNoCommand
  sealed trait YesNoCommand extends Command

  object Yes extends YesNoCommand
  object No extends YesNoCommand


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[YesNoCommands.type], classOf[YesNoCommand])

}
