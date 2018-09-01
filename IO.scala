package utils

import scala.io.StdIn

object IO {

  def clearScreen(): Unit = {
    val nBlankLines = 100
    println(System.lineSeparator() * nBlankLines)
  }


  def display(rows: Seq[Seq[String]], header: Seq[String] = Seq(), reverse: Boolean = false): String = {
    val verticalDividerFillCharacter = "-"
    val verticalDividerColumnSeparator = "+"
    val newlineStrings = Seq("\n", "\r\n", "\r")
    val newlineReplacement = " "
    val unifiedRowsWithHeaderIfNonEmpty = header.headOption.map(_ => rows.+:(rows.headOption.map(x => header.padTo(x.length, new String)).getOrElse(header))).getOrElse(rows)
      .map(_.map(newlineStrings.foldLeft(_)((x, y) => x.replace(y, newlineReplacement))))
    val lengthToNStringsColumns = unifiedRowsWithHeaderIfNonEmpty.transpose.map(_.map(_.length).groupBy(x => x).mapValues(_.length))
    val maxLengths = lengthToNStringsColumns.map(x => if(x(x.keys.max) < x.filterKeys(_ != x.keys.max).values.sum) x.keys.max else x.keys.max + 1)
    val verticalDivider = maxLengths.map(verticalDividerFillCharacter * _).mkString(verticalDividerColumnSeparator, verticalDividerColumnSeparator,
      verticalDividerColumnSeparator)
    def displayRow(row: Seq[String], maxLengths: Seq[Int], columnSeparator: String = "|"): String = {
      val space = " "
      row.zip(maxLengths).map(x => x._1 + space * (x._2 - x._1.length)).mkString(columnSeparator, columnSeparator, columnSeparator)
    }
    Some(header.headOption.map(_ => Seq(verticalDivider, displayRow(unifiedRowsWithHeaderIfNonEmpty.head, maxLengths), verticalDivider)).getOrElse(Seq(verticalDivider))
      .++(header.headOption.map(_ => unifiedRowsWithHeaderIfNonEmpty.tail).getOrElse(unifiedRowsWithHeaderIfNonEmpty).map(displayRow(_, maxLengths))).:+(verticalDivider))
      .map(x => if(reverse) x.reverse else x).get.mkString(System.lineSeparator())
  }


  def promptToPressEnterAndWait(leadWithNewLine: Boolean = true): Unit ={
    if(leadWithNewLine) println
    println("Press enter to continue")
    StdIn.readLine()
  }

}
