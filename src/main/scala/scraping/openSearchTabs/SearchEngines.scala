package scraping.openSearchTabs

import scraping.search.SearchEngine
import utils.enumerated.{Enumerated, SelfNamed}

object SearchEngines extends Enumerated {

  override type T = SearchEngine
  sealed abstract class SearchEngine(encodedQueryToUrl: String => String) extends SelfNamed{

    def queryToUrl(query: String): String = {
      encodedQueryToUrl(SearchEngine.encodeForUrl(query))
    }

  }

  object Bing extends SearchEngine(x => s"https://www.bing.com/search?q=$x")


  override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[SearchEngines.type], classOf[SearchEngine])

}
