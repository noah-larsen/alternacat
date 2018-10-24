package utils.io

object Display {

  def indent(value: String, nTabs: Int = 1): String = {
    val indentString = "\t"
    indentString * nTabs + value
  }


  def indentLines(input: String, nTabs: Int = 1): String = {
    input.split(System.lineSeparator()).map(indent(_, nTabs)).mkString(System.lineSeparator())
  }


  def table(rows: Seq[Seq[String]], header: Seq[String] = Seq(), reverse: Boolean = false): String = {
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


  def withColonSpace(value: String): String = {
    val suffix = ": "
    value + suffix
  }


  def withCommaSpaces: Seq[String] => String = {
    val separator = ", "
    _.mkString(separator)
  }


  def withCommaSpaces(values: String*): String = {
    withCommaSpaces(values.toSeq)
  }


  def withNewlines: Seq[String] => String = {
    val separator = System.lineSeparator()
    _.mkString(separator)
  }


  def withNewlines(values: String*): String = {
    withNewlines(values.toSeq)
  }


  def withPipes: Seq[String] => String = {
    val separator = "|"
    _.mkString(separator)
  }


  def withPipes(values: String*): String = {
    withNewlines(values.toSeq)
  }


  def withSemicolonSpaces: Seq[String] => String = {
    val separator = "; "
    _.mkString(separator)
  }


  def withSemicolonSpaces(values: String*): String = {
    withSemicolonSpaces(values.toSeq)
  }


  def withSpacedArrows: Seq[String] => String = {
    val separator = " -> "
    _.mkString(separator)
  }


  def withSpacedArrows(values: String*): String = {
    withSpacedArrows(values.toSeq)
  }


  def withSpacedDashes: Seq[String] => String = {
    val separator = " - "
    _.mkString(separator)
  }


  def withSpacedDashes(values: String*): String = {
    withSpacedDashes(values.toSeq)
  }


  def withSpacedGreaterThans: Seq[String] => String = {
    val separator = " > "
    _.mkString(separator)
  }


  def withSpacedGreaterThans(values: String*): String = {
    withSpacedGreaterThans(values.toSeq)
  }


  def withSpaces: Seq[String] => String = {
    val separator = " "
    _.mkString(separator)
  }


  def withSpaces(values: String*): String = {
    withSpaces(values.toSeq)
  }


  def withTabs: Seq[String] => String = {
    val separator = "\t"
    _.mkString(separator)
  }


  def withTabs(values: String*): String = {
    withTabs(values.toSeq)
  }


  def withTabsToSpaces(input: String, tabWidth: Int = defaultTabWidth): String = {
    val tab = "\t"
    val space = " "
    input.replace(tab, space * tabWidth)
  }


  def wordWrap(input: String, maxNCharactersPerLine: Int = defaultMaxNCharactersPerLine, tabWidth: Int = defaultTabWidth): String = {
    val wordLineBreakCharacter = "-"
    val normalized = withTabsToSpaces(input, tabWidth)
    val (line, remainder) = normalized match {
      case x if x.length <= maxNCharactersPerLine => (x, new String)
      case _ =>
        val lineToTruncate = normalized.substring(0, Math.min(maxNCharactersPerLine + 1, normalized.length))
        lineToTruncate.indexWhere(_.toString == System.lineSeparator()) match {
          case x if x != -1 => (normalized.substring(0, x + 1), normalized.substring(x + 1))
          case _ =>
            lineToTruncate.lastIndexWhere(_.isSpaceChar) match {
              case x if x != -1 => (normalized.substring(0, x + 1), normalized.substring(x + 1))
              case _ => (normalized.substring(0, maxNCharactersPerLine - 1) + wordLineBreakCharacter, normalized.substring(maxNCharactersPerLine - 1))
            }
        }
    }
    if(remainder.isEmpty) line else line + System.lineSeparator() + wordWrap(remainder, maxNCharactersPerLine, tabWidth)
  }


  private def defaultMaxNCharactersPerLine: Int = {
    75
  }


  private def defaultTabWidth: Int = {
    4
  }

}
