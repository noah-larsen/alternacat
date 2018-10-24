package utils.exceptions

import utils.enumerated.SelfNameable
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords
import utils.io.Display

abstract class SelfDescribed(
                              parameters: Seq[Seq[String]] = Seq(),
                              cause: Option[Throwable] = None,
                              includeCauseMessage: Boolean = true
                            ) extends Exception(cause.orNull) with SelfNameable {

  def this(parameters: Seq[String]){
    this(Seq(parameters))
  }


  def this(parameter: String){
    this(Seq(Seq(parameter)))
  }


  override def getMessage: String = {
    (name(ObjectNameWithSpacesBetweenWords()) match {
      case x if parameters.isEmpty => x
      case x => Display.withColonSpace(x)
    }) + Display.withSemicolonSpaces(cause.filter(_ => !includeCauseMessage).map(x => parameters ++ Seq(Seq(x.getMessage))).getOrElse(parameters).map(Display.withCommaSpaces))
  }

}
