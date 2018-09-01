package utils.commands


abstract class Command(
                        parameters: Seq[Parameter] = Seq(),
                        specifiedLetterName: Option[Char] = None
                      ) extends AbstractCommand(parameters) {

  def letterName: Char = {
    specifiedLetterName.getOrElse(name.head.toLower)
  }


  override def usage: String = {
    usage(letterName.toString)
  }

}
