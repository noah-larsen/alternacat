package scraping.search

import java.net.{URL, URLEncoder}
import java.nio.charset.StandardCharsets

import net.ruippeixotog.scalascraper.browser.HtmlUnitBrowser

import scala.util.Try

trait SearchEngine {

  def search(htmlUnitBrowser: HtmlUnitBrowser, query: String, maxNResults: Int): Try[Seq[URL]]


  protected def parse(input: String, precursor: String, successor: String, disqualifyingSubstrings: Seq[String] = Seq()): Seq[String] = {
    input.split(precursor).tail.map(x => (x, x.indexOf(successor))).collect{case x if x._2 != -1 => x._1.substring(0, x._2)}.filter(x => !disqualifyingSubstrings.exists(x
      .contains))
  }


  protected def parseNegation(input: String, precursor: String, successor: String): Seq[String] = {
    val tokens = input.split(precursor)
    tokens.headOption.map(Seq(_) ++ tokens.tail.map(x => (x, x.indexOf(successor))).collect{
      case x if x._2 != -1 => x._1.substring(x._2 + successor.length)
      case x => x._1
    }).getOrElse(Seq())
  }

}

object SearchEngine {

  def encodeForUrl(value: String): String = {
    URLEncoder.encode(value, StandardCharsets.UTF_8.name())
  }

}
