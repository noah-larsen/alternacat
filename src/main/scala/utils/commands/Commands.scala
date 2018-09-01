package utils.commands

import utils.IO
import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, InvalidParametersException, UnknownCommandException}
import utils.commands.IndexedCommand.{IndexCommand, IndexListCommand}
import utils.enumerated.Enumerated

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait Commands extends Enumerated {

  type CommandType <: Command
  override type T = CommandType


  def promptUntilParsed[T](indexCommandItems: Map[Int, T] = Map[Int, T](), without: Seq[CommandType] = Seq(), showUsage: Boolean = true, leadWithNewline: Boolean = true)
                          (implicit clearScreenUponSuccess: Boolean = true): CommandInvocation[CommandType, T] = {
    val withoutComplete = without ++ indexedCommand.filter(_ => indexCommandItems.isEmpty).map(Seq(_)).getOrElse(Nil)
    if(showUsage) println((if(leadWithNewline) System.lineSeparator() else new String) + usage(withoutComplete))
    def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if Option(x).forall(_.trim.isEmpty) => readLineUntilNonEmpty case x => x}
    parse(readLineUntilNonEmpty, indexCommandItems, withoutComplete).recover{case e: CommandException =>
      println(e.message.trim + System.lineSeparator())
      promptUntilParsed(indexCommandItems, withoutComplete, showUsage = false)
    }.get match {
      case x =>
        if(clearScreenUponSuccess) IO.clearScreen()
        x
    }
  }


  def usage(without: Seq[CommandType] = Seq()): String = {
    indexedCommand.map(Seq(_)).getOrElse(Nil).++(letterCommands).filter(!without.contains(_)).map(_.usage).mkString(System.lineSeparator())
  }


  def usageWithLeadingNewline(without: Seq[CommandType] = Seq()): String = {
    System.lineSeparator() + usage(without)
  }


  protected def letterCommands: Seq[CommandType] = {
    values.filter(!_.isInstanceOf[IndexedCommand])
  }


  protected def indexedCommand: Option[CommandType with IndexedCommand] = {
    values.collectFirst{case x: Command with IndexedCommand => x.asInstanceOf[CommandType with IndexedCommand]}
  }


  private def parse[T](line: String, indexedCommandValues: Map[Int, T] = Map(), without: Seq[CommandType] = Seq()): Try[CommandInvocation[CommandType, T]] = {

    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)


    letterCommands
      .find(_.letterName.toString == tokens.head).filter(!without.contains(_)).map(CommandInvocation[CommandType, T](_, tokens.tail).validate)
      .orElse(
        indexedCommand.collect {
          case _: IndexCommand =>
            indexedCommand.filter(!without.contains(_)).map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(x => (x._1, x._2.get)).filter(x => indexedCommandValues
              .contains(x._2)).map(x => CommandInvocation(x._1, tokens.tail, Some(indexedCommandValues(x._2))).validate)
          case _: IndexListCommand =>
            indexedCommand.filter(!without.contains(_)).map((_, tokens.map(y => Try(y.toInt)))).filter(_._2.forall(_.isSuccess)).map(x => (x._1, x._2.map(_.get))).filter(_._2
              .forall(x => indexedCommandValues.contains(x))).map(x => Try(CommandInvocation[CommandType, T](x._1, Seq(), None, Some(x._2.map(indexedCommandValues(_))))))
        }.flatten
      )
      .getOrElse(Failure[CommandInvocation[CommandType, T]](UnknownCommandException)).asInstanceOf[Try[CommandInvocation[CommandType, T]]]

  }


//  private[utils] case class CommandInvocation[T](
//                                   command: CommandType,
//                                   arguments: Seq[String],
//                                   indexCommandSelection: Option[T] = None,
//                                   indexListCommandSelection: Option[Seq[T]] = None
//                                 ){
//
//    def validate: Try[CommandInvocation[T]] = {
//      Unit match {
//        case _ if !(parameters.length - command.nUnrequiredParameters to parameters.collectFirst{case _: ListParameter[_] => Int.MaxValue}.getOrElse(parameters.length))
//          .contains(arguments.length) => Failure(IncorrectNumberParametersException)
//        case _ if parameterToArguments.exists(x => x._1 match {
//          case y: ValueParameter[_] => y.parse(x._2.head).isFailure
//          case y: OptionalParameter[_] => y.parse(x._2.head).isFailure
//          case y: ListParameter[_] => y.parse(x._2).isFailure
//        }) => Failure(InvalidParametersException)
//        case _ => Try(this)
//      }
//    }
//
//
//    def value[U](valueParameter: ValueParameter[U]): U = {
//      (parameters.indexOf(valueParameter), valueParameter.default) match {
//        case (x, Some(y)) if !arguments.indices.contains(x) => y
//        case (x, _) => valueParameter.parse(arguments(x)).get
//      }
//    }
//
//
//    def value[U](optionalParameter: OptionalParameter[U]): Option[U] = {
//      Some(parameters.indexOf(optionalParameter))
//        .filter(arguments.indices.contains)
//        .map(x => optionalParameter.parse(arguments(x)).get)
//    }
//
//
//    def value[U](listParameter: ListParameter[U]): Seq[U] = {
//      listParameter.parse(arguments.slice(command.parameters.indexOf(listParameter), arguments.length)).get
//    }
//
//
//    private def parameterToArguments: Map[Parameter, Seq[String]] = {
//      parameters.lastOption.map{
//        case x: ValueParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
//        case x: OptionalParameter[_] => parameters.zip(arguments).map(x => (x._1, Seq(x._2))).toMap
//        case x: ListParameter[_] => parameters.init.zip(arguments).map(x => (x._1, Seq(x._2))).toMap + (x -> arguments.slice(parameters.length - 1, arguments.length))
//      }.getOrElse(Map())
//    }
//
//
//    private def parameters: Seq[Parameter] = {
//      command.parameters
//    }
//
//  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object InvalidParametersException extends CommandException("Invalid Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
