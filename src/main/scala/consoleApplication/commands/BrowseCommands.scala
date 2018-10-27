package consoleApplication.commands

import utils.commands.{Command, Commands}

object BrowseCommands extends Commands {

  override type CommandType = BrowseCommand
  sealed abstract class BrowseCommand extends Command

  object SourceNodes extends BrowseCommand
  object TargetNodes extends BrowseCommand

  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[BrowseCommands.type], classOf[BrowseCommand])

}
