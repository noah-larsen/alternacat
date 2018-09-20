package utils.enumerated

import utils.enumerated.SelfNamed.NameFormats.CaseFormats.{CaseFormat, FirstLetterLowercase, Lowercase, Uppercase}
import utils.enumerated.SelfNamed.NameFormats._

trait SelfNameable {

  def name(nameFormat: NameFormat): String = {

    def withCaseFormat(name: String, caseFormat: CaseFormat): String = {
      caseFormat match {
        case FirstLetterLowercase => name.headOption.map(_.toLower + name.tail).getOrElse(name)
        case Lowercase => name.toLowerCase
        case CaseFormats.None => name
        case Uppercase => name.toUpperCase
      }
    }


    def words(camelCase: String): Seq[String] = {
      Some(camelCase.zipWithIndex.filter(x => x._1.isUpper || x._2 == 0).map(_._2)).map(x => x.zip(x.tail.:+(camelCase.length)).map(y => camelCase.substring(y._1, y._2))).get
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

}
