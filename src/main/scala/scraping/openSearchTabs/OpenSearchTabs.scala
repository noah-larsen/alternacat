package scraping.openSearchTabs

import org.apache.commons.lang3.SystemUtils

import scala.sys.process.Process

object OpenSearchTabs {

  def open(queries: Seq[String], searchEngine: SearchEngines.SearchEngine): Unit ={
    queries.map(searchEngine.queryToUrl).foreach{ url =>
      Process(url match {
        case x if SystemUtils.IS_OS_MAC_OSX => s"open $x"
        case _ => throw new UnsupportedOperationException
      }).!!
    }
  }

}
