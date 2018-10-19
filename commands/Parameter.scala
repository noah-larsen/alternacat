package utils.commands

import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.FirstLetterLowercase

import scala.util.Try

sealed abstract class Parameter extends SelfNamed(FirstLetterLowercase)


object Parameter {

  abstract class ValueParameter[T](
                               val parse: String => Try[T],
                               val default: Option[T] = None
                             ) extends Parameter


  abstract class ListParameter[T](
                                   val parse: Seq[String] => Try[Seq[T]]
                                 ) extends Parameter


  abstract class OptionalParameter[T](
                                       val parse: String => Try[T]
                                     ) extends Parameter


  abstract class OptionParameter(
                                  val letter: Char
                                ) extends Parameter


  abstract class StringParameter(default: Option[String] = None) extends ValueParameter(Try(_), default)
  abstract class StringsParameter extends ListParameter(Try(_))
  abstract class OptionalStringParameter extends OptionalParameter(Try(_))

}
