package scraping.proxies

import java.net.InetSocketAddress

import com.typesafe.config.Config
import scraping.proxies.Fate0ProxyListEntry.Fate0ProxyListEntryFields._
import utils.enumerated.SelfNamed
import utils.enumerated.SelfNamed.NameFormats.CaseFormats.Lowercase
import utils.enumerated.SelfNamed.NameFormats.ObjectNameWithUnderscoresBetweenWords

import collection.convert.ImplicitConversions._

case class Fate0ProxyListEntry(
                                anonymity: String,
                                country: Option[String],
                                exportAddress: Seq[String],
                                from: String,
                                host: String,
                                port: Int,
                                responseTime: Double,
                                tpe: String,
                              ){

  def alpha2CountryCode: Option[String] = {
    country
  }


  def isHighAnonymous: Boolean = {
    val highAnonymousAnonymity = "high_anonymous"
    anonymity.equalsIgnoreCase(highAnonymousAnonymity)
  }


  def isHttpBased: Boolean = {
    val httpTpeSubstring = "http"
    tpe.toLowerCase.contains(httpTpeSubstring.toLowerCase)
  }

}

object Fate0ProxyListEntry {

  def apply(config: Config): Fate0ProxyListEntry = {
    Fate0ProxyListEntry(
      anonymity = config.getString(Anonymity.name),
      country = if(config.hasPath(Country.name)) Some(config.getString(Country.name)) else None,
      exportAddress = config.getStringList(ExportAddress.name),
      from = config.getString(From.name),
      host = config.getString(Host.name),
      port = config.getInt(Port.name),
      responseTime = config.getDouble(ResponseTime.name),
      tpe = config.getString(Type.name),
    )
  }

  object Fate0ProxyListEntryFields {
    sealed abstract class Fate0ProxyListEntryField extends SelfNamed(ObjectNameWithUnderscoresBetweenWords(Lowercase))
    object Anonymity extends Fate0ProxyListEntryField
    object Country extends Fate0ProxyListEntryField
    object ExportAddress extends Fate0ProxyListEntryField
    object From extends Fate0ProxyListEntryField
    object Host extends Fate0ProxyListEntryField
    object Port extends Fate0ProxyListEntryField
    object ResponseTime extends Fate0ProxyListEntryField
    object Type extends Fate0ProxyListEntryField
  }

}