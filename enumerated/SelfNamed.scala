package utils.enumerated

import utils.enumerated.SelfNamed.NameFormats
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.{CaseFormat, FirstLetterLowercase, Lowercase, Uppercase}
import utils.enumerated.SelfNamed.NameFormats._

abstract class SelfNamed(nameFormat: NameFormat = ObjectName()) {

  def this(caseFormat: CaseFormat){
    this(ObjectName(caseFormat))
  }


  def name: String = {

    def withCaseFormat(name: String, caseFormat: CaseFormat): String = {
      caseFormat match {
        case FirstLetterLowercase => name.headOption.map(_.toLower + name.tail).getOrElse(name)
        case Lowercase => name.toLowerCase
        case CaseFormats.None => name
        case Uppercase => name.toUpperCase
      }
    }


    val multipleClassNameAddedDisambiguationSymbol = "$"
    val classNameSeparatorRE = "[.$]"
    val space = " "
    val underscore = "_"
    val objectName = getClass.getName.split(classNameSeparatorRE).filter(_.nonEmpty).last
    nameFormat match {
      case Custom(x) => x
      case ObjectName(x) => withCaseFormat(objectName, x)
      case ObjectNameWithSpacesBetweenWords(x) => withCaseFormat(words(objectName).mkString(space), x)
      case ObjectNameWithUnderscoresBetweenWords(x) => withCaseFormat(words(objectName).mkString(underscore), x)
    }
  }


  private def words(camelCase: String): Seq[String] = {
    Some(camelCase.zipWithIndex.filter(x => x._1.isUpper || x._2 == 0).map(_._2)).map(x => x.zip(x.tail.:+(camelCase.length)).map(y => camelCase.substring(y._1, y._2))).get
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