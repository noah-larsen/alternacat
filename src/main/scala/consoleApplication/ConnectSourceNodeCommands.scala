package consoleApplication

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands._

import scala.util.Try

object ConnectSourceNodeCommands extends Commands {

  override type CommandType = ConnectSourceNodeCommand
  sealed abstract class ConnectSourceNodeCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object RelatedTargetNodes extends ConnectSourceNodeCommand
  object BrowseTargetNodes extends ConnectSourceNodeCommand(specifiedLetterName = Some('t'))
  object SearchTargetNodes extends ConnectSourceNodeCommand(Seq(Keyword))
  object Descendants extends ConnectSourceNodeCommand(Seq(MaxDepth))
  object Kin extends ConnectSourceNodeCommand(Seq(MaxDepth))
  object Next extends ConnectSourceNodeCommand(Seq(FinishedValue1To5))
  object Back extends ConnectSourceNodeCommand


  object FinishedValue1To5 extends OptionalParameter(x => Try(x.toInt).filter(finishedValues.contains))


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodeCommands.type], classOf[ConnectSourceNodeCommand])

}
