package consoleApplication.commands

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import consoleApplication.finishedValues
import utils.commands.Parameter.OptionalParameter
import utils.commands._

import scala.util.Try

object ConnectSourceNodeCommands extends Commands {

  override type CommandType = ConnectSourceNodeCommand
  sealed abstract class ConnectSourceNodeCommand(parameters: Seq[Parameter] = Seq(), specifiedLetterName: Option[Char] = None) extends Command(parameters, specifiedLetterName)

  object RelatedTargetNodes extends ConnectSourceNodeCommand
  object GoToTargetNodes extends ConnectSourceNodeCommand(specifiedLetterName = Some('t'))
  object SearchTargetNodes extends ConnectSourceNodeCommand(Seq(Keyword))
  object Descendants extends ConnectSourceNodeCommand(Seq(MaxDepth))
  object Kin extends ConnectSourceNodeCommand(Seq(MaxDepth))
  object Next extends ConnectSourceNodeCommand(Seq(FinishedValue1To5))
  object Back extends ConnectSourceNodeCommand


  object FinishedValue1To5 extends OptionalParameter(x => Try(x.toInt).filter(finishedValues.contains))


  override protected def help: (ConnectSourceNodeCommand) => String = {
    case RelatedTargetNodes => "Displays the nodes from the target forest that are related to this source node."
    case GoToTargetNodes =>
      "Moves to the target forest, which can be traversed and edited, including setting which target nodes are related to the source node under consideration here."
    case SearchTargetNodes =>
      "Performs a text-based search over the names of nodes in the target forest. If applicable, matching nodes can be traversed to and set as related to the source node " +
        "under consideration here."
    case Descendants => s"Displays the nodes underneath this node in the source forest, up to ${MaxDepth.name} levels down."
    case Kin => ""
    case Next => ""
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodeCommands.type], classOf[ConnectSourceNodeCommand])

}
