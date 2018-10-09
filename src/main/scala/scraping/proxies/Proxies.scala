package scraping.proxies

import java.net.InetSocketAddress

import com.gargoylesoftware.htmlunit.ProxyConfig
import net.ruippeixotog.scalascraper.browser.{HtmlUnitBrowser, JsoupBrowser}

import scala.util.Try

case class Proxies(alpha2CountryCode: Option[String] = None) {

  def highlyAnonymousProxies: Try[Stream[Proxy]] = {
    Fate0ProxyList.get.map(_.filter(x => x.isHighAnonymous && alpha2CountryCode.forall(y => x.alpha2CountryCode.exists(_.equalsIgnoreCase(y)))).filter(_.isHttpBased).sortBy(_
      .responseTime).map(x => Proxy(x.host, x.port)).toStream)
  }


  def highlyAnonymousBrowsers: Try[Stream[HtmlUnitBrowser]] = {
    highlyAnonymousProxies.map(_.map(x => new HtmlUnitBrowser(proxy = new ProxyConfig(x.host, x.port))))
  }


  case class Proxy(host: String, port: Int)

}

