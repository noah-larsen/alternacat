package utils.commands

import utils.commands.Commands.{IncorrectNumberParametersException, InvalidParametersException}
import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}

import scala.util.{Failure, Try}

case class CommandInvocation[T <: Command, U](
                                                command: T,
                                                arguments: Seq[String],
                                                indexCommandSelection: Option[U] = None,
                                                indexListCommandSelection: Option[Seq[U]] = None
                                              ){

  def validate: Try[CommandInvocation[T, U]] = {
    Unit match {
      case _ if !(parameters.length - command.nUnrequiredParameters to parameters.collectFirst{case _: ListParameter[_] => Int.MaxValue}.getOrElse(parameters.length))
        .contains(arguments.length) => Failure(IncorrectNumberParametersException)
      case _ if parameterToArguments.exists(x => x._1 match {
        case y: ValueParameter[_] => y.parse(x._2.head).isFailure
        case y: OptionalParameter[_] => y.parse(x._2.head).isFailure
        case y: ListParameter[_] => y.parse(x._2).isFailure
      }) => Failure(InvalidParametersException)
      case _ => Try(this)
    }
  }


  def value[V](valueParameter: ValueParameter[V]): V = {
    (parameters.indexOf(valueParameter), valueParameter.default) match {
      case (x, Some(y)) if !arguments.indices.contains(x) => y
      case (x, _) => valueParameter.parse(arguments(x)).get
    }
  }


  def value[V](optionalParameter: OptionalParameter[V]): Option[V] = {
    Some(parameters.indexOf(optionalParameter))
      .filter(arguments.indices.contains)
      .map(x => optionalParameter.parse(arguments(x)).get)
  }


  def value[V](listParameter: ListParameter[V]): Seq[V] = {
    listParameter.parse(arguments.slice(command.parameters.indexOf(listParameter), arguments.length)).get
  }


  private def parameterToArguments: Map[Parameter, Seq[String]] = {
    parameters.lastOption.map{
      case x: ValueParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
      case x: OptionalParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
      case x: ListParameter[_] => parameters.init.zip(arguments).map(x => (x._1, Seq(x._2))).toMap + (x -> arguments.slice(parameters.length - 1, arguments.length))
    }.getOrElse(Map())
  }


  private def parameters: Seq[Parameter] = {
    command.parameters
  }

}