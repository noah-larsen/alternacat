package utils.exceptions

import utils.enumerated.SelfNameable
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithSpacesBetweenWords
import utils.io.Display

abstract class SelfDescribed(
                              parameters: Seq[Seq[String]] = Seq(Seq()),
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
    Display.withColonSpace(name(ObjectNameWithSpacesBetweenWords())) + Display.withSemicolonSpaces((parameters ++ cause.filter(_ => !includeCauseMessage).map(x => Seq(Seq(x.getMessage))).getOrElse(Seq(Seq()))).map(Display.withCommaSpaces))
  }

}
