package utils.commands

import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.FirstLetterLowercase

import scala.util.Try

sealed abstract class Parameter extends SelfNamed(FirstLetterLowercase)


object Parameter {

  abstract class ValueParameter[T](
                               val parse: String => Try[T] = (x: String) => Try(x.asInstanceOf[T]),
                               val default: Option[T] = None
                             ) extends Parameter


  abstract class ListParameter[T](
                                   val parse: Seq[String] => Try[Seq[T]] = (x: Seq[String]) => Try(x.map(_.asInstanceOf[T]))
                                 ) extends Parameter


  abstract class OptionalParameter[T](
                                       val parse: String => Try[T] = (x: String) => Try(Some(x).asInstanceOf[T])
                                     ) extends Parameter


}
