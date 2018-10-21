package utils.commands

import utils.commands.Parameter.{ListParameter, OptionalParameter, ValueParameter}
import utils.commands.Commands.{CommandException, IncorrectNumberParametersException, InvalidParametersException, UnknownCommandException}
import utils.commands.IndexedCommand.{IndexCommand, IndexListCommand}
import utils.enumerated.Enumerated
import utils.io.{Display, IO}

import scala.collection.Map.WithDefault
import scala.io.StdIn
import scala.util.{Failure, Success, Try}

trait Commands extends Enumerated {

  type CommandType <: Command
  override type T = CommandType


  def promptUntilParsed[T](indexCommandItems: Map[Int, T] = Map[Int, T](), without: Seq[CommandType] = Seq(), showUsage: Boolean = true, leadWithNewline: Boolean = true)
                          (implicit clearScreenUponSuccess: Boolean = true): CommandInvocation[CommandType, T] = {

    def promptUntilParsedInternal(indexCommandItems: Map[Int, T] = Map[Int, T](), without: Seq[CommandType] = Seq(), showUsage: Boolean = true,
                                  leadWithNewline: Boolean = true): Either[CommandInvocation[CommandType, T], CommandType => String] = {
      if(showUsage) println((if(leadWithNewline) System.lineSeparator() else new String) + usage(without))
      def readLineUntilNonEmpty: String = StdIn.readLine() match {case x if Option(x).forall(_.trim.isEmpty) => readLineUntilNonEmpty case x => x}
      commandInvocationOrHelp(readLineUntilNonEmpty, indexCommandItems, without).recover{case e: CommandException =>
        println(e.message.trim + System.lineSeparator())
        promptUntilParsedInternal(indexCommandItems, without, showUsage = false)
      }.get
    }


    val withoutComplete = without ++ indexedCommand.filter(_ => indexCommandItems.isEmpty).map(Seq(_)).getOrElse(Nil)
    promptUntilParsedInternal(indexCommandItems, withoutComplete, showUsage, leadWithNewline) match {
      case Left(commandInvocation) =>
        if(clearScreenUponSuccess) IO.clearScreen()
        commandInvocation
      case Right(commandToHelpMessage) =>
        IO.clearScreen()
        val maxWidth = 80
        println(commands.filter(x => commandToHelpMessage(x).nonEmpty).map(y => Display.withSpacedDashes(y.name, new String) + System.lineSeparator() + Display.indentLines(
          Display.wordWrap(commandToHelpMessage(y), maxWidth)) + System.lineSeparator()).mkString(System.lineSeparator()))
        promptUntilParsed(indexCommandItems, withoutComplete, showUsage)
    }

  }


  def usage(without: Seq[CommandType] = Seq()): String = {
    (commands ++ helpOption.map(_ => Seq(Help)).getOrElse(Nil)).filter(!without.contains(_)).map(_.usage).mkString(System.lineSeparator())
  }


  def usageWithLeadingNewline(without: Seq[CommandType] = Seq()): String = {
    System.lineSeparator() + usage(without)
  }


  protected def help: CommandType => String = _ => new String


  private def helpOption: Option[CommandType => String] = {
    Some(help).filter(x => commands.exists(x(_).nonEmpty) && !commands.exists(_.letterName == Help.letterName))
  }


  protected def commands: Seq[CommandType] = {
    indexedCommand.map(Seq(_)).getOrElse(Nil).++(letterCommands)
  }


  protected def letterCommands: Seq[CommandType] = {
    values.filter(!_.isInstanceOf[IndexedCommand])
  }


  protected def indexedCommand: Option[CommandType with IndexedCommand] = {
    values.collectFirst{case x: Command with IndexedCommand => x.asInstanceOf[CommandType with IndexedCommand]}
  }


  private def commandInvocationOrHelp[T](line: String, indexedCommandValues: Map[Int, T] = Map(), without: Seq[CommandType] = Seq()): Try[Either[CommandInvocation[
    CommandType, T], CommandType => String]] = {
    val whitespaceRe = "\\s+"
    val tokens = line.split(whitespaceRe)
    letterCommands.filter(!without.contains(_))
      .find(_.letterName.toString == tokens.head).map(x => CommandInvocation[CommandType, T](x, tokens.tail).validate.map(Left(_)))
      .orElse(
        indexedCommand.collect {
          case x if x.isInstanceOf[IndexCommand] =>
            indexedCommand.filter(!without.contains(_)).map((_, Try(tokens.head.toInt))).filter(_._2.isSuccess).map(y => (y._1, y._2.get)).filter(y => indexedCommandValues
              .contains(y._2)).map(y => CommandInvocation(y._1, tokens.tail, Some(indexedCommandValues(y._2))).validate.map(Left(_)))
          case x if x.isInstanceOf[IndexListCommand] =>
            indexedCommand.filter(!without.contains(_)).map((_, tokens.map(y => Try(y.toInt)))).filter(_._2.forall(_.isSuccess)).map(y => (y._1, y._2.map(_.get))).filter(_._2
              .forall(y => indexedCommandValues.contains(y))).map(y => Try(Left(CommandInvocation[CommandType, T](y._1, Seq(), None, Some(y._2.map(
              indexedCommandValues(_)))))))
        }.flatten
      )
      .orElse(helpOption.filter(_ => Help.letterName.toString == tokens.head).map(x => Try(Right(x))))
      .getOrElse(Failure(UnknownCommandException)).asInstanceOf[Try[Either[CommandInvocation[CommandType, T], CommandType => String]]]
  }


  private object Help extends Command

}

object Commands {

  case class CommandException(message: String) extends RuntimeException(message)
  object IncorrectNumberParametersException extends CommandException("Incorrect Number of Parameters")
  object InvalidParametersException extends CommandException("Invalid Parameters")
  object UnknownCommandException extends CommandException("Unknown Command")

}
