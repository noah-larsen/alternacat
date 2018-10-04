package scraping.search

import java.net.{URL, URLEncoder}
import java.nio.charset.StandardCharsets

import net.ruippeixotog.scalascraper.browser.{Browser, HtmlUnitBrowser}

object DuckDuckGo extends Search {

  def search(htmlUnitBrowser: HtmlUnitBrowser, query: String, maxNResults: Int): Seq[URL] = {
    val linkPrecursor = "href=\""
    val resultLinkSuccessor = "\"><span class=\"result__url__domain\">"
    val disqualifyingSubstring = ">"
    val disqualifyingHostSubstring = "duckduckgo.com"
    val url = s"https://duckduckgo.com/?q=${encodeForUrl(query)}&t=h_&ia=web"
    val document = htmlUnitBrowser.get(url)
    //todo
    val html = document.toHtml
    parse(html, linkPrecursor, resultLinkSuccessor, Seq(disqualifyingSubstring)).map(new URL(_)).filter(!_.getHost.contains(disqualifyingHostSubstring))
  }

}
