package consoleApplication

import java.io.{File, PrintWriter}

import connectedForests.{DevelopingConnectedForests, LabeledForest}
import consoleApplication.BrowseCommands.{SourceNodes, TargetNodes}
import consoleApplication.LookupTargetNodesCommands._
import consoleApplication.CommonParameters.{Keyword, MaxDepth}
import consoleApplication.ConnectSourceNodeCommands.{BackToMainMenu, _}
import consoleApplication.ConnectSourceNodesSelectionCommands.{SelectAll, SelectNodes}
import consoleApplication.OtherCommands.InitializeGoogleProductTaxonomy
import consoleApplication.MainCommands._
import consoleApplication.SearchResultCommands.GoToResultNumber
import org.rogach.scallop.{ScallopConf, ScallopOption}
import persistence.{ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat, PathToIdJsonFormat, StringJsonFormat}
import play.api.libs.json.Json
import taxonomies.GoogleProductTaxonomy
import utils.IO
import utils.commands.{Commands, IndexedCommand}
import utils.commands.Parameter.ListParameter
import utils.enumerated.SelfNamed

import scala.io.Source
import scala.util.Try


object Driver extends App {

  case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val persistencePathname: ScallopOption[String] = opt[String](required = true)
    val fromForest: ScallopOption[String] = opt[String](required = true)
    val toForest: ScallopOption[String] = opt[String](required = true)
    verify()
  }


  private val conf = Conf(args)
  private val persistencePathname = conf.persistencePathname()
  private val sourceForest = conf.fromForest()
  private val targetForest = conf.toForest()
  private val jsonFormat = ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat(StringJsonFormat, StringJsonFormat)
  private val captializeFirstLetterRelatedNodes = true


  main(if(new File(persistencePathname).exists()) DevelopingConnectedForests(jsonFormat.fromJson(Json.parse(Source.fromFile(persistencePathname).mkString))) else
    DevelopingConnectedForests().withForest(sourceForest).withForest(targetForest), promptWithLeadingNewline = false)


  private def main(dcfs: DCFS, promptWithLeadingNewline: Boolean = true): Unit = {
    val commandInvocation = MainCommands.promptUntilParsed(leadWithNewline = promptWithLeadingNewline)
    commandInvocation.command match {
      case Connect => main(connectSelection(dcfs, finishedProportion(commandInvocation.value(MaxFinishedValue1To5))))
      case Browse => main(browse(dcfs))
      case Other => main(other(dcfs))
      case Save =>
        new PrintWriter(persistencePathname){write(jsonFormat.toJson(dcfs.connectedForestsAndRelatedNodesToFinishedProportion).toString()); close()}
        main(dcfs)
      case Quit =>
    }
  }


  private def connectSelection(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double): DCFS = {
    val header = "Unfinished Source Node With No Unfinished Ancestors"
    val indexToUnfinishedSubroots = IndexedCommand.withOneBasedIndexes(dcfs.unfinishedSubroots(sourceForest, targetForest, maxFinishedProportionUnfinishedNode))
    println(IndexedCommand.display(indexToUnfinishedSubroots.mapValues(display(_)), header))
    val commandInvocation = ConnectSourceNodesSelectionCommands.promptUntilParsed(indexToUnfinishedSubroots, if(indexToUnfinishedSubroots.isEmpty) Seq(SelectAll) else Nil)
    commandInvocation.command match {
      case SelectNodes => connect(dcfs, maxFinishedProportionUnfinishedNode, commandInvocation.indexListCommandSelection)
      case SelectAll => connect(dcfs, maxFinishedProportionUnfinishedNode)
      case ConnectSourceNodesSelectionCommands.Back =>
        dcfs
    }
  }


  private def connect(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double, selectedSubroots: Option[Seq[Seq[String]]] = None): DCFS = {

    def process(dcfs_continue: (DCFS, Boolean), unfinishedSubroot: Seq[String]): (DCFS, Boolean) = {
      val (dcfs, continue) = (dcfs_continue._1, dcfs_continue._2)
      println(display(unfinishedSubroot))
      val commandInvocation = ConnectSourceNodeCommands.promptUntilParsed()
      commandInvocation.command match {
        case LookupTargetNodes => process((lookupTargetNodes(dcfs, unfinishedSubroot), continue), unfinishedSubroot)
        case SearchTargetNodes => process((search(dcfs, unfinishedSubroot, commandInvocation.value(Keyword)), continue), unfinishedSubroot)
        case RelatedNodes =>
          println(displayRelatedNodes(dcfs, unfinishedSubroot) + System.lineSeparator())
          process(dcfs_continue, unfinishedSubroot)
        case Descendants =>
          println(displayDescendants(dcfs, sourceForest, unfinishedSubroot, commandInvocation.value(MaxDepth)) + System.lineSeparator())
          process(dcfs_continue, unfinishedSubroot)
        case Next => dcfs_continue
        case Finish => (dcfs.withFinishedProportion(sourceForest, unfinishedSubroot, targetForest, finishedProportion(commandInvocation.value(FinishedValue1To5))), continue)
        case ConnectSourceNodeCommands.BackToMainMenu => (dcfs, false)
      }
    }


    val unfinishedSubroots = Some(dcfs.unfinishedSubroots(sourceForest, targetForest, maxFinishedProportionUnfinishedNode)).map(x => selectedSubroots.map(y => x.filter(z => y
      .exists(z.startsWith(_)))).getOrElse(x)).get
    unfinishedSubroots.foldLeft((dcfs, true)){(x, y) => if(x._2) process(x, y) else x} match {
      case x if x._2 && unfinishedSubroots.nonEmpty => connect(x._1, maxFinishedProportionUnfinishedNode, selectedSubroots)
      case x => x._1
    }

  }


  private def browse(dcfs: DCFS): DCFS = {
    val commandInvocation = BrowseCommands.promptUntilParsed()
    commandInvocation.command match {
      case SourceNodes => browseSourceNodes(dcfs)
      case TargetNodes => ???
    }
  }


  private def browseSourceNodes(dcfs: DCFS, sourceNode: Option[Seq[String]] = None): DCFS = {

    def editRelatedNodes(dcfs: DCFS, sourceNode: Seq[String]): DCFS = {
      println(display(sourceNode))
      val commandInvocation = EditRelatedNodesCommands.promptUntilParsed()
      commandInvocation.command match {
        case EditRelatedNodesCommands.LookupTargetNodes => editRelatedNodes(lookupTargetNodes(dcfs, sourceNode), sourceNode)
        case EditRelatedNodesCommands.SearchTargetNodes => editRelatedNodes(search(dcfs, sourceNode, commandInvocation.value(Keyword)), sourceNode)
        case EditRelatedNodesCommands.RelatedNodes =>
          println(displayRelatedNodes(dcfs, sourceNode) + System.lineSeparator())
          editRelatedNodes(dcfs, sourceNode)
        case EditRelatedNodesCommands.Descendants =>
          println(displayDescendants(dcfs, sourceForest, sourceNode, commandInvocation.value(MaxDepth)) + System.lineSeparator())
          editRelatedNodes(dcfs, sourceNode)
        case EditRelatedNodesCommands.Back => dcfs
      }
    }


    val indexToChildrenOrRoots = IndexedCommand.withOneBasedIndexes(sourceNode.map(dcfs.childPaths(sourceForest, _)).getOrElse(dcfs.rootPaths(sourceForest)).toSeq)
    sourceNode.foreach(x => println(display(x) + System.lineSeparator()))
    if(indexToChildrenOrRoots.nonEmpty) println(IndexedCommand.display(indexToChildrenOrRoots.mapValues(display(_, sourceNode.getOrElse(Nil))), sourceNode.map(_ => NodeTypes
      .Child.name).getOrElse(NodeTypes.Root.name)))
    sourceNode.foreach(x => println(System.lineSeparator() + display(x)))
    val without = Seq(
      (BrowseSourceNodesCommands.GoUp, sourceNode.isEmpty),
      (BrowseSourceNodesCommands.RelatedNodes, sourceNode.isEmpty),
      (BrowseSourceNodesCommands.EditRelatedNodes, sourceNode.isEmpty),
    ).filter(_._2).map(_._1)
    val commandInvocation = BrowseSourceNodesCommands.promptUntilParsed(indexToChildrenOrRoots, without)
    commandInvocation.command match {
      case BrowseSourceNodesCommands.GoTo => browseSourceNodes(dcfs, commandInvocation.indexCommandSelection)
      case BrowseSourceNodesCommands.GoUp => browseSourceNodes(dcfs, sourceNode.collect{case x if x.length > 1 => x.init})
      case BrowseSourceNodesCommands.RelatedNodes =>
        println(displayRelatedNodes(dcfs, sourceNode.get))
        IO.promptToPressEnterAndWait()
        IO.clearScreen()
        browseSourceNodes(dcfs, sourceNode)
      case BrowseSourceNodesCommands.EditRelatedNodes => browseSourceNodes(editRelatedNodes(dcfs, sourceNode.get), sourceNode)
      case BrowseSourceNodesCommands.BackToMainMenu => dcfs
    }

  }


  private def other(dcfs: DCFS): DCFS = {
    val commandInvocation = OtherCommands.promptUntilParsed()
    commandInvocation.command match {
      case InitializeGoogleProductTaxonomy =>
        val forestLabel = commandInvocation.arguments.tail.head
        dcfs.withForest(forestLabel).withPaths(forestLabel, GoogleProductTaxonomy(commandInvocation.arguments.head).labeledForest.paths)
      case OtherCommands.Back => dcfs
    }
  }


  private def lookupTargetNodes(dcfs: DCFS, sourceForestNode: Seq[String], targestForestNode: Option[Seq[String]] = None): DCFS = {
    val indexToChildrenOrRoots = IndexedCommand.withOneBasedIndexes(targestForestNode.map(dcfs.childPaths(targetForest, _)).getOrElse(dcfs.rootPaths(targetForest)).toSeq)
    if(indexToChildrenOrRoots.nonEmpty) println(IndexedCommand.display(indexToChildrenOrRoots.mapValues(display(_, targestForestNode.getOrElse(Nil))), targestForestNode
      .map(_ => NodeTypes.Child.name).getOrElse(NodeTypes.Root.name)))
    val displaySourceNodePrefix = "(Source Node: "
    val displaySourceNodeSuffix = ")"
    targestForestNode.foreach(x => println(System.lineSeparator() + display(x)))
    println(System.lineSeparator() + displaySourceNodePrefix + display(sourceForestNode) + displaySourceNodeSuffix)
    val without = Seq(
      (GoUp, targestForestNode.isEmpty),
      (MarkRelated, targestForestNode.forall(dcfs.relatedNodes(sourceForest, sourceForestNode, targetForest).contains)),
      (RemoveRelatedness, targestForestNode.forall(!dcfs.relatedNodes(sourceForest, sourceForestNode, targetForest).contains(_)))
    ).filter(_._2).map(_._1)
    val commandInvocation = LookupTargetNodesCommands.promptUntilParsed(indexToChildrenOrRoots, without)
    commandInvocation.command match {
      case GoTo => lookupTargetNodes(dcfs, sourceForestNode, commandInvocation.indexCommandSelection)
      case GoUp => lookupTargetNodes(dcfs, sourceForestNode, targestForestNode.collect{case x if x.length > 1 => x.init})
      case MarkRelated => lookupTargetNodes(dcfs.withRelationship(sourceForest, sourceForestNode, targetForest, targestForestNode.get), sourceForestNode, targestForestNode)
      case RemoveRelatedness => lookupTargetNodes(dcfs.withoutRelationship(sourceForest, sourceForestNode, targetForest, targestForestNode.get), sourceForestNode, targestForestNode)
      case CreateNewTargetNodeHere =>
        val partsNameSeparator = " "
        lookupTargetNodes(dcfs.withPath(targetForest, targestForestNode.getOrElse(Nil) :+ Some(commandInvocation.value(PartOfName).mkString(partsNameSeparator)).map(x => if(
          captializeFirstLetterRelatedNodes) x.capitalize else x).get), sourceForestNode, targestForestNode)
      case LookupTargetNodesCommands.Back => dcfs
    }
  }


  private def search(dcfs: DCFS, sourceForestNode: Seq[String], query: Seq[String]): DCFS = {
    val keywordsSeparator = " "
    val header = "Search Result"
    val maxNResults = 100
    val indexToResult = IndexedCommand.withOneBasedIndexes(dcfs.resultPathToNormalizedScore(targetForest, query.mkString(keywordsSeparator),
      maxNResults).toSeq.sortBy(-_._2).map(_._1))
    println(IndexedCommand.display(indexToResult.mapValues(display(_)), header, reverse = true))
    val commandInvocation = SearchResultCommands.promptUntilParsed(indexToResult)
    commandInvocation.command match {
      case GoToResultNumber => lookupTargetNodes(dcfs, sourceForestNode, commandInvocation.indexCommandSelection)
      case SearchResultCommands.Search => search(dcfs, sourceForestNode, commandInvocation.value(Keyword))
      case SearchResultCommands.Back => dcfs
    }
  }


  private def displayRelatedNodes(dcfs: DCFS, sourceForestNode: Seq[String]): String = {
    val indent = "  "
    val noRelatedNodesSymbol = "()"
    IO.display(LabeledForest.subPaths(sourceForestNode).zip(dcfs.relatedNodesOfPath(sourceForest, sourceForestNode, targetForest)).map(x => Seq(display(x._1)) ++ Some(x._2
      .map(display(_))).filter(_.nonEmpty).getOrElse(Seq(noRelatedNodesSymbol)).map(indent + _)).flatMap(_.map(Seq(_))))
  }


  private def displayDescendants(dcfs: DCFS, forestLabel: String, node: Seq[String], maxDepth: Int): String = {
    IO.display(dcfs.pathsSubtree(forestLabel, node).-(node).filter(_.length - node.length <= maxDepth).toSeq.sortWith((x, y) => x.length < y.length && y.startsWith(x) || x
      .zip(y).find(z => z._1 != z._2).exists(z => z._1.compare(z._2) < 0)).map(x => Seq(display(x, node))))
  }


  private def finishedProportion(finishedValue: Int): Double = {
    (finishedValue - finishedValues.min).toDouble / (finishedValues.max - finishedValues.min).toDouble
  }


  private def display[N](path: Seq[N], withoutSubpath: Seq[N] = Nil): String = {
    val separator = " -> "
    path.drop(withoutSubpath.inits.toList.find(path.startsWith(_)).get.length).mkString(separator)
  }


  private object NodeTypes {
    object Root extends SelfNamed
    object Child extends SelfNamed
  }

}
