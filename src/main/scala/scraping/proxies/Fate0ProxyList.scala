package scraping.proxies

import com.typesafe.config.ConfigFactory

import scala.io.Source
import scala.util.Try

object Fate0ProxyList {

  def get: Try[Seq[Fate0ProxyListEntry]] = {
    Try(Source.fromURL(url).getLines().map(x => Fate0ProxyListEntry(ConfigFactory.parseString(x))).toSeq)
  }


  private val url = "https://raw.githubusercontent.com/fate0/proxylist/master/proxy.list"

}
