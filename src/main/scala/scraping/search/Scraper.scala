package scraping.search

import java.net.URL

import net.ruippeixotog.scalascraper.browser.HtmlUnitBrowser

case class Scraper(
                    searchEngine: SearchEngine,
                    browsers: () => Seq[HtmlUnitBrowser]
                  ) {

  def scrape[T, U](queries: Map[T, String], maxNResults: Int, resultF: (Seq[URL], HtmlUnitBrowser) => U, includeAds: Boolean = false): Map[T, U] = {

    searchEngine.search(???, ???, maxNResults, includeAds)
    queries.map(x => (x._1, resultF(???, ???)))
  }

}
