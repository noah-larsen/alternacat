package taxonomies.drivers

import java.net.URLDecoder
import java.nio.charset.StandardCharsets

import utils.io.{Display, IO}


object ScrapeAmazonBrowseNodes extends App {
  val url = "http://demo.amazonnodes.com/"
  val outputPathname = "io/taxonomies/amazonBrowseNodes.txt"
  val substringsUrlsToNotOpenLinksOf = Seq("C%23/4163516b6634634e", "C%23/427373716534514e3162532b4b7663")
  val urlSeparator = "/"
  val documents = utils_.WebsiteCrawler.crawl(url, url, substringsUrlsToNotOpenLinksOf = substringsUrlsToNotOpenLinksOf)
  val unopenableUrls = documents.collect{case Right(x) => x.toString}
  if(unopenableUrls.nonEmpty){
    println(System.lineSeparator() + "Error Urls:" + System.lineSeparator())
    println(unopenableUrls.mkString(System.lineSeparator()))
  }
  val urls = documents.map(_.fold(_.location, _.toString))
  val categories = urls.map(_.substring(url.length).split(urlSeparator).filter(_.nonEmpty).zipWithIndex.filter(_._2 % 2 == 0).map(x => URLDecoder.decode(x._1, StandardCharsets
    .UTF_8.name())).toList).filter(_.nonEmpty)
  IO.write(outputPathname, categories.map(Display.withSpacedGreaterThans).mkString(System.lineSeparator()))
}

