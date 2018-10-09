package taxonomies

import connectedForests.{ConnectedForests, LabeledForest}

import scala.io.Source

case class ProductTaxonomy(categories: Seq[Seq[String]]) {

  def labeledForest: LabeledForest[String] = {
    LabeledForest(categories)
  }

}

object ProductTaxonomy {

  def parseFromGreaterThanSeparatedFile(pathname: String): ProductTaxonomy = {
    val commentSymbol = "#"
    val subcategorySeparator = ">"
    ProductTaxonomy(Source.fromFile(pathname).getLines().map(_.trim).filter(!_.startsWith(commentSymbol)).map(_.split(subcategorySeparator).map(_.trim).toList).toList)
  }

}