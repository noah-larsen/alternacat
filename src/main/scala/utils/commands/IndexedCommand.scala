package utils.commands

import utils.io.{Display, IO}

sealed trait IndexedCommand extends AbstractCommand {

}

object IndexedCommand {

  trait IndexCommand extends IndexedCommand {

    override def usage: String = {
      val nameSymbol = indexSymbol
      usage(nameSymbol)
    }

  }


  trait IndexListCommand extends IndexedCommand {

    override def usage: String = {
      val nameSymbol = s"$indexSymbol [$indexSymbol ...]"
      usage(nameSymbol)
    }

  }


  def withOneBasedIndexes[T](values: Seq[T]): Map[Int, T] = {
    values.zipWithIndex.map(x => (x._2 + 1, x._1)).toMap
  }


  def display(indexToValue: Map[Int, String], valueHeaderDescription: String, reverse: Boolean = false): String = {
    Display.table(indexToValue.toSeq.sortBy(_._1).map(x => Seq(x._1.toString, x._2)), Some(valueHeaderDescription).map(Seq(indexSymbol, _)).getOrElse(Seq()), reverse)
  }


  private val indexSymbol = "#"

}