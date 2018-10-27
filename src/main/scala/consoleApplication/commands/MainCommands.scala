package consoleApplication.commands

import consoleApplication.finishedValues
import utils.commands.Parameter.ValueParameter
import utils.commands.{Command, Commands, Parameter}

import scala.util.Try

object MainCommands extends Commands {

  override type CommandType = MainCommand
  sealed abstract class MainCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object Connect extends MainCommand(Seq(MaxFinishedValue1To5))
  object Browse extends MainCommand
  object Other extends MainCommand
  object Quit extends MainCommand


  object MaxFinishedValue1To5 extends ValueParameter(x => Try(x.toInt).filter(finishedValues.contains), Some(1))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[MainCommands.type], classOf[MainCommand])

}
