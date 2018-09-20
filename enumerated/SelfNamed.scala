package utils.enumerated

import utils.enumerated.SelfNamed.NameFormats
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.{CaseFormat, FirstLetterLowercase, Lowercase, Uppercase}
import utils.enumerated.SelfNamed.NameFormats._

abstract class SelfNamed(nameFormat: NameFormat = ObjectName()) extends SelfNameable {

  def this(caseFormat: CaseFormat){
    this(ObjectName(caseFormat))
  }


  def name: String = {
    name(nameFormat)
  }

}

object SelfNamed {

  object NameFormats {

    sealed trait NameFormat


    case class Custom(name: String) extends NameFormat
    case class ObjectName(caseFormat: CaseFormat = CaseFormats.None) extends NameFormat
    case class ObjectNameWithUnderscoresBetweenWords(caseFormat: CaseFormat = CaseFormats.None) extends NameFormat
    case class ObjectNameWithSpacesBetweenWords(caseFormat: CaseFormat = CaseFormats.None) extends NameFormat


    object CaseFormats {
      sealed trait CaseFormat
      object FirstLetterLowercase extends CaseFormat
      object Lowercase extends CaseFormat
      object None extends CaseFormat
      object Uppercase extends CaseFormat
    }

  }

}