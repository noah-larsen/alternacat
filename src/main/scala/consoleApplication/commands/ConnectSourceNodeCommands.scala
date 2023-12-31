package consoleApplication.commands

import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import consoleApplication.commands.MainCommands.MaxFinishedValue1To5
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


  val relatedTargetNodesDescription = "Displays the nodes from the target forest that are related to this source node."
  val goToTargetNodesDescription = "Moves to the target forest, which can be traversed and edited, including setting which target nodes are related to the source node under " +
    "consideration here."
  val searchTargetNodesDescription = "Performs a text-based search over the names of nodes in the target forest. If applicable, matching nodes can be traversed to and set " +
    "as related to the source node under consideration here."
  val descendantsHelpDescription = s"Displays the nodes underneath this node in the source forest, up to ${MaxDepth.name} levels down."


  override protected def help: (ConnectSourceNodeCommand) => String = {
    case RelatedTargetNodes => relatedTargetNodesDescription
    case GoToTargetNodes => goToTargetNodesDescription
    case SearchTargetNodes => searchTargetNodesDescription
    case Descendants => descendantsHelpDescription
    case Kin => s"Displays the node's siblings and their ancestors, up to ${MaxDepth.name} levels up."
    case Next =>
      s"Moves to the next source node available from the source nodes selected for processing and their descendants. The node's finished value is set to ${
        FinishedValue1To5.name} if specified, and retains its current value otherwise. For a given ${MaxFinishedValue1To5.name}, descendants of this source node cannot be " +
        s"processed until a higher ${FinishedValue1To5.name} is set for this source node."
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[ConnectSourceNodeCommands.type], classOf[ConnectSourceNodeCommand])

}
