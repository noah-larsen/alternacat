package utils.commands

import utils.commands.Commands.{IncorrectNumberParametersException, InvalidParametersException}
import utils.commands.Parameter.{ListParameter, OptionParameter, OptionalParameter, ValueParameter}

import scala.util.{Failure, Try}

case class CommandInvocation[T <: Command, U](
                                                command: T,
                                                arguments: Seq[String],
                                                indexCommandSelection: Option[U] = None,
                                                indexListCommandSelection: Option[Seq[U]] = None
                                              ){

  def validate: Try[CommandInvocation[T, U]] = {
    Unit match {
      case _ if !(positionalParameters.length - command.nUnrequiredPositionalParameters to positionalParameters.collectFirst{case _: ListParameter[_] => Int.MaxValue}
        .getOrElse(positionalParameters.length)).contains(positionalArguments.length) => Failure(IncorrectNumberParametersException)
      case _ if positionalParameterToArguments.exists(x => x._1 match {
        case y: ValueParameter[_] => y.parse(x._2.head).isFailure
        case y: OptionalParameter[_] => y.parse(x._2.head).isFailure
        case y: ListParameter[_] => y.parse(x._2).isFailure
        case _ => false
      }) || options.exists(!parameters.collect{case x: OptionParameter => x.letter}.contains(_)) => Failure(InvalidParametersException)
      case _ => Try(this)
    }
  }


  def value[V](valueParameter: ValueParameter[V]): V = {
    (positionalParameters.indexOf(valueParameter), valueParameter.default) match {
      case (x, Some(y)) if !arguments.indices.contains(x) => y
      case (x, _) => valueParameter.parse(arguments(x)).get
    }
  }


  def value[V](optionalParameter: OptionalParameter[V]): Option[V] = {
    Some(positionalParameters.indexOf(optionalParameter))
      .filter(arguments.indices.contains)
      .map(x => optionalParameter.parse(arguments(x)).get)
  }


  def value[V](listParameter: ListParameter[V]): Seq[V] = {
    listParameter.parse(arguments.slice(command.parameters.indexOf(listParameter), arguments.length)).get
  }


  def value(optionParameter: OptionParameter): Boolean = {
    options.contains(optionParameter.letter)
  }


  private def options: Set[Char] = {
    optionsAndPositionalArguments._1
  }


  private def parameters: Seq[Parameter] = {
    command.parameters
  }


  private def positionalParameters: Seq[Parameter] = {
    parameters.filter(!_.isInstanceOf[OptionParameter])
  }


  private def positionalParameterToArguments: Map[Parameter, Seq[String]] = {
    parameters.lastOption.map{
      case x: ValueParameter[_] => positionalParameters.zip(positionalArguments).map(x => (x._1, Seq(x._2))).toMap
      case x: OptionalParameter[_] => positionalParameters.zip(positionalArguments).map(x => (x._1, Seq(x._2))).toMap
      case x: ListParameter[_] => positionalParameters.init.zip(positionalArguments).map(x => (x._1, Seq(x._2))).toMap + (x -> arguments.slice(parameters.length - 1, arguments
        .length))
      case x: OptionParameter => Map[Parameter, Seq[String]]()
    }.getOrElse(Map())
  }


  private def positionalArguments: Seq[String] = {
    optionsAndPositionalArguments._2
  }


  private def optionsAndPositionalArguments: (Set[Char], Seq[String]) = {
    val optionPrefix = "-"
    val optionArguments = arguments.takeWhile(_.startsWith(optionPrefix))
    (optionArguments.flatMap(_.substring(optionPrefix.length)).toSet, arguments.drop(optionArguments.length))
  }

}