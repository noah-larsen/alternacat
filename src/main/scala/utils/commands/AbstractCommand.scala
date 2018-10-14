package utils.commands

import utils.commands.Parameter.{ListParameter, OptionParameter, OptionalParameter, ValueParameter}
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.{CaseFormats, ObjectNameWithSpacesBetweenWords}

abstract class AbstractCommand(parameters_ : Seq[Parameter]) extends SelfNamed(ObjectNameWithSpacesBetweenWords(CaseFormats.Lowercase)) {

  def usage: String


  def parameters: Seq[Parameter] = {
    optionParameters ++ requiredParameters ++ unrequiredPositionalParameters ++ listParameter.map(Seq(_)).getOrElse(Nil)
  }


  def nUnrequiredPositionalParameters: Int = {
    unrequiredPositionalParameters.length
  }


  protected def usage(nameSymbol: String): String = {
    val nameSymbolNameSeparator = " - "
    val descriptionParametersSeparator = ": "
    val parametersSeparator = " "
    val listParameterSuffix = " ..."
    val unrequiredParameterPrefix = "["
    val defaultParameterNameDefaultValueSeparator = " = "
    val optionParameterLetterNameSeparator = " - "
    val unrequiredParameterSuffix = "]"
    nameSymbol + nameSymbolNameSeparator + name + parameters.headOption.map(_ => descriptionParametersSeparator + parameters.map {
      case x: OptionParameter => unrequiredParameterPrefix + x.letter + optionParameterLetterNameSeparator + x.name + unrequiredParameterSuffix
      case x: ValueParameter[_] => x.default.map(unrequiredParameterPrefix + x.name + defaultParameterNameDefaultValueSeparator + _ + unrequiredParameterSuffix).getOrElse(x
        .name)
      case x: OptionalParameter[_] => unrequiredParameterPrefix + x.name + unrequiredParameterSuffix
      case x: ListParameter[_] => x.name + parametersSeparator + unrequiredParameterPrefix + x.name + listParameterSuffix + unrequiredParameterSuffix
    }.mkString(parametersSeparator)).getOrElse(new String)
  }


  private def optionParameters: Seq[OptionParameter] = {
    parameters_.collect{case x: OptionParameter => x}
  }


  private def requiredParameters: Seq[ValueParameter[_]] = {
    parameters_.collect{case x: ValueParameter[_] if x.default.isEmpty => x}
  }


  private def unrequiredPositionalParameters: Seq[Parameter] = {
    parameters_.collect{
      case x: ValueParameter[_] if x.default.isDefined => x
      case x: OptionalParameter[_] => x
    }
  }


  private def listParameter: Option[ListParameter[_]] = {
    parameters_.collectFirst{case x: ListParameter[_] => x}.filter(_ => unrequiredPositionalParameters.isEmpty)
  }

}

