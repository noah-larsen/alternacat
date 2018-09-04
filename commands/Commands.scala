package utils.commands

import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, InvalidParametersException, UnknownCommandException}
import utils.commands.IndexedCommand.{IndexCommand, IndexListCommand}
import utils.enumerated.Enumerated
import utils.io.IO

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
          case x if x.isInstanceOf[IndexCommand] =>
            indexedCommand.filter(!without.contains(_)).map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(y => (y._1, y._2.get)).filter(y => indexedCommandValues
              .contains(y._2)).map(y => CommandInvocation(y._1, tokens.tail, Some(indexedCommandValues(y._2))).validate)
          case x if x.isInstanceOf[IndexListCommand] =>
            indexedCommand.filter(!without.contains(_)).map((_, tokens.map(y => Try(y.toInt)))).filter(_._2.forall(_.isSuccess)).map(y => (y._1, y._2.map(_.get))).filter(_._2
              .forall(y => indexedCommandValues.contains(y))).map(y => Try(CommandInvocation[CommandType, T](y._1, Seq(), None, Some(y._2.map(indexedCommandValues(_))))))
        }.flatten
      )
      .getOrElse(Failure[CommandInvocation[CommandType, T]](UnknownCommandException)).asInstanceOf[Try[CommandInvocation[CommandType, T]]]
  }

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object InvalidParametersException extends CommandException("Invalid Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
