package utils.io

import java.io.{BufferedWriter, FileWriter, PrintWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

import scala.io.StdIn
import scala.util.Try

object IO {

  def append(pathname: String, text: String, addNewLineToEnd: Boolean = true): Try[Unit] = {
    Try(Files.write(Paths.get(pathname), (if(addNewLineToEnd) text + System.lineSeparator() else text).getBytes(StandardCharsets.UTF_8), StandardOpenOption.WRITE,
      StandardOpenOption.CREATE, StandardOpenOption.APPEND))
  }


  def clearScreen(): Unit = {
    val nBlankLines = 100
    println(System.lineSeparator() * nBlankLines)
  }


  def promptToPressEnterAndWait(leadWithNewLine: Boolean = true): Unit ={
    if(leadWithNewLine) println
    println("Press enter to continue")
    StdIn.readLine()
  }


  def write(pathname: String, text: String): Try[Unit] = {
    Try(Files.write(Paths.get(pathname), text.getBytes(StandardCharsets.UTF_8)))
  }

}
