package utils_

import java.net.URL

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model.Document
import org.jsoup.HttpStatusException

import scala.util.Try

object WebsiteCrawler {

  def crawl(urlStart: String, urlPrefix: String, visitedUrls: Set[String] = Set(), substringsUrlsToNotOpenLinksOf: Seq[String] = Seq()): Seq[Either[Document, URL]] ={
    println(urlStart)
    Try {
      val externalUrlSubstring = "://"
      val rootPrefix = "/"
      val browser = JsoupBrowser()
      val document = browser.get(urlStart)
      val links = document >> elementList("a") >> attr("href")
      val relevantLinks = links.map {
        case x if x.contains(externalUrlSubstring) => x
        case x if x.startsWith(rootPrefix) =>
          val url_ = new URL(urlStart)
          val host = url_.getHost
          url(url_.toString.substring(0, url_.toString.indexOf(host) + host.length), x)
        case x => url(urlStart, x)
      }.filter(x => x.startsWith(urlPrefix) && !(visitedUrls + urlStart).contains(x))
      document.location match {
        case x if substringsUrlsToNotOpenLinksOf.exists(x.contains) => Seq(Left(document))
        case _ => Seq(Left(document)) ++ relevantLinks.flatMap(crawl(_, urlPrefix, visitedUrls + urlStart ++ relevantLinks, substringsUrlsToNotOpenLinksOf))
      }
    }.recover{case e: HttpStatusException => Seq(Right(new URL(urlStart)))}.get
  }


  private def url(prefix: String, suffix: String): String = {
    val separator = "/"
    (prefix, suffix) match {
      case x if x._1.endsWith(separator) && x._2.startsWith(separator) => x._1 + x._2.tail
      case x if !x._1.endsWith(separator) && !x._2.startsWith(separator) => x._1 + separator + x._2
      case x => x._1 + x._2
    }
  }

}
