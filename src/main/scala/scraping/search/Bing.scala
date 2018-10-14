package scraping.search
import java.net.URL

import net.ruippeixotog.scalascraper.browser.HtmlUnitBrowser

import scala.util.Try

case class Bing(includeAds: Boolean = false) extends SearchEngine {

  override def search(htmlUnitBrowser: HtmlUnitBrowser, query: String, maxNResults: Int): Try[Seq[URL]] = Try {

    def url(firstResultN: Int) = s"https://www.bing.com/search?q=${SearchEngine.encodeForUrl(query)}&first=$firstResultN&FORM=PERE"


    def possibleUrlsWithProtocol(uri: String): Seq[String] = {
      val protocolUrnSeparator = "://"
      val possibleProtocols = Seq("http", "https")
      if(uri.contains(protocolUrnSeparator)) Seq(uri) else possibleProtocols.map(_ + protocolUrnSeparator + uri)
    }


    def search(results: Seq[URL] = Seq()): Seq[URL] = {
      val precursor = "<cite"
      val successor = "</cite>"
      val tagPrefix = "<"
      val tagSuffix = ">"
      val linkPrecursor = "<a href=\""
      val linkSuccessor = "\""
      val adUrlPrefix = "https://www.bing.com/aclick"
      val document = htmlUnitBrowser.get(url(results.length + 1))
      val html = document.toHtml
      val cites = parse(html, precursor, successor).map{
        case x if parse(x, linkPrecursor, linkSuccessor).nonEmpty => parse(x, linkPrecursor, linkSuccessor).headOption
        case x if x.startsWith(tagSuffix) => Some(parseNegation(x.tail, tagPrefix, tagSuffix).mkString)
        case x =>
          //todo
          println(s"Cannot parse: $x")
          None
      }.collect{case Some(x) if !x.startsWith(adUrlPrefix) || !includeAds => x}
      val links = parse(html, linkPrecursor, linkSuccessor)
      val newResults = cites.map(x => links.filter(y => possibleUrlsWithProtocol(x).exists(y.startsWith))).collect{case x if x.nonEmpty => x.maxBy(_.length)}.map(new URL(_))
      (results ++ newResults).distinct match {
        case x if x.length < maxNResults && newResults.nonEmpty => search(x)
        case x => x.take(maxNResults)
      }
    }


    search()

  }

}
