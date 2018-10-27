package consoleApplication.commands

import utils.commands.Parameter.StringParameter
import utils.commands.{Command, Commands, Parameter}

object OtherCommands extends Commands {

  override type CommandType = OtherCommand
  sealed abstract class OtherCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object InitializeProductTaxonomy extends OtherCommand(Seq(Pathname, ForestLabel))
  object Back extends OtherCommand


  object Pathname extends StringParameter
  object ForestLabel extends StringParameter


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[OtherCommands.type], classOf[OtherCommand])

}
