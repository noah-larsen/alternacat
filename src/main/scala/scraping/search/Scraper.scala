package scraping.search

import java.net.URL

import net.ruippeixotog.scalascraper.browser.HtmlUnitBrowser
import net.ruippeixotog.scalascraper.browser.HtmlUnitBrowser.HtmlUnitDocument
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import scala.collection.immutable.HashMap
import scala.util.{Random, Try}

case class Scraper(
                    searchEngine: SearchEngine,
                    browsers: Stream[HtmlUnitBrowser],
                    maxNResultsPerSearch: Int,
                    maxNAttemptsPerResult: Int
                  ) {

  def scrape[T](queries: Seq[String], resultF: (URL, HtmlUnitBrowser) => T): Try[Seq[Seq[Try[T]]]] = Try {
    queries.map(x => execute(browsers, searchEngine.search(_, x, maxNResultsPerSearch).map(_.map(x => execute(browsers, resultF(x, _), Some(maxNAttemptsPerResult))))).flatten
      .get)
  }


  def scrapeDocuments[T](queries: Seq[String], resultF: HtmlUnitDocument => T): Try[Seq[Seq[Try[T]]]] = {
    scrape(queries, (x, y) => resultF(y.get(x.toString)))
  }


  def scrapeText[T](queries: Seq[String], resultF: String => T): Try[Seq[Seq[Try[T]]]] = {
    scrapeDocuments(queries, (x: HtmlUnitDocument) => resultF(x >> allText))
  }


  private def execute[T](browsers: Stream[HtmlUnitBrowser], f: HtmlUnitBrowser => T, maxNAttempts: Option[Int] = None): Try[T] = {
    val evaluated = browsers.map(x => Try(f(x))) match {case x => maxNAttempts.map(x.take).getOrElse(x)}
    evaluated.find(_.isSuccess).getOrElse(evaluated(Random.nextInt(evaluated.length)))
  }

}
