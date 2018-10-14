package taxonomies.drivers

import scraping.openSearchTabs.OpenSearchTabs
import scraping.openSearchTabs.SearchEngines.Bing
import utils.io.Display

object Search extends App {
  val productSubQuery = Display.withSpaces(args)
  val queries = Seq(
    s"uses of $productSubQuery",
    s"alternative uses for $productSubQuery",
    s"reasons to buy $productSubQuery",
    s"why you should buy $productSubQuery",
    s"things you can do with $productSubQuery"
  )
  OpenSearchTabs.open(queries, Bing)
}
