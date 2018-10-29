package consoleApplication.commands

import consoleApplication.CommonParameters.Keyword
import utils.commands.IndexedCommand.IndexCommand
import utils.commands.{Command, Commands, Parameter}

object SearchResultCommands extends Commands {

  override type CommandType = SearchResultCommand
  sealed abstract class SearchResultCommand(parameters: Seq[Parameter] = Seq()) extends Command(parameters)

  object GoToResultNumber extends SearchResultCommand with IndexCommand
  object Search extends SearchResultCommand(Seq(Keyword))
  object Back extends SearchResultCommand


  override protected def help: (SearchResultCommand) => String = {
    case GoToResultNumber => ""
    case Search => ""
    case Back => ""
  }


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[SearchResultCommands.type], classOf[SearchResultCommand])

}
