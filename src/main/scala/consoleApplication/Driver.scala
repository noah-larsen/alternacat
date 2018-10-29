package consoleApplication

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths
import java.time.{ZoneId, ZoneOffset, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import connectedForests.{DevelopingConnectedForests, LabeledForest}
import consoleApplication.commands.BrowseCommands.{SourceNodes, TargetNodes}
import consoleApplication.commands.GoToTargetNodesCommands.{CreateNewRelatedTargetChildNode, _}
import consoleApplication.CommonParameters.{Keyword, MaxDepth, PartOfName}
import consoleApplication.commands.ConnectSourceNodeCommands.{Back, _}
import consoleApplication.commands.ConnectSourceNodesSelectionCommands.{SelectAll, SelectNodes}
import consoleApplication.Driver.ForestTypes
import consoleApplication.Driver.ForestTypes.ForestType
import consoleApplication.commands._
import consoleApplication.commands.EditNameCommands.Edit
import consoleApplication.commands.OtherCommands.{ForestLabel, InitializeProductTaxonomy, Pathname}
import consoleApplication.commands.MainCommands._
import consoleApplication.commands.SearchResultCommands.GoToResultNumber
import consoleApplication.commands.YesNoCommands.{No, Yes}
import org.rogach.scallop.{ScallopConf, ScallopOption}
import persistence.{ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat, PathToIdJsonFormat, StringJsonFormat}
import play.api.libs.json.Json
import taxonomies.ProductTaxonomy
import utils.commands.{Command, CommandInvocation, Commands, IndexedCommand}
import utils.commands.Parameter.ListParameter
import utils.enumerated.{Enumerated, SelfNamed}
import utils.exceptions.SelfDescribed
import utils.io.{Display, IO}

import scala.io.Source
import scala.util.Try


object Driver extends App {

  case class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val sourceForest: ScallopOption[String] = opt[String](required = true)
    val targetForest: ScallopOption[String] = opt[String](required = true)
    val persistencePathname: ScallopOption[String] = opt[String](required = true)
    val logPath: ScallopOption[String] = opt[String]()
    val admin: ScallopOption[Boolean] = opt[Boolean](default = Some(false), hidden = true)
    val doNotSave: ScallopOption[Boolean] = opt[Boolean](default = Some(false))
    verify()
  }


  private val conf = Conf(args)
  private val sourceForest = conf.sourceForest()
  private val targetForest = conf.targetForest()
  private val persistencePathname = conf.persistencePathname()
  private val logPath = conf.logPath.toOption
  private val admin = conf.admin()
  private val autoSave = !conf.doNotSave()


  private val jsonFormat = ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat(StringJsonFormat, StringJsonFormat)
  private val captializeFirstLetterRelatedNodes = true
  private val abbreviationToTargetNodeNameSubstring = Map(
    "g" -> "Get a gift for",
    "gl" -> "Get a gift for someone who likes",
    "s" -> "Save money",
    "so" -> "Save money on"
  )


  IO.clearScreen()
  main(if(new File(persistencePathname).exists()) DevelopingConnectedForests[String, String](jsonFormat.fromJson(Json.parse(Source.fromFile(persistencePathname)
    .mkString))) else DevelopingConnectedForests[String, String]().withForest(sourceForest).withForest(targetForest))


  private def main(dcfs: DCFS): Unit = {
    val commandInvocation = MainCommands.promptUntilParsed(without = if(admin) Nil else Seq(Browse, Other))
    log(commandInvocation)
    commandInvocation.command match {
      case Connect => main(connectSelection(dcfs, finishedProportion(commandInvocation.value(MaxFinishedValue1To5))))
      case Browse => main(browse(dcfs))
      case Other => main(other(dcfs))
      case Quit =>
    }
  }


  private def connectSelection(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double): DCFS = {
    val header = "Unfinished Source Node With No Unfinished Ancestors"
    val indexToUnfinishedSubroots = IndexedCommand.withOneBasedIndexes(dcfs.unfinishedSubroots(sourceForest, targetForest, maxFinishedProportionUnfinishedNode))
    val display_ = IndexedCommand.display(indexToUnfinishedSubroots.mapValues(display(_)), header)
    val commandInvocation = ConnectSourceNodesSelectionCommands.promptUntilParsed(display_, indexToUnfinishedSubroots, if(indexToUnfinishedSubroots.isEmpty) Seq(
      SelectAll) else Nil)
    log(commandInvocation)
    commandInvocation.command match {
      case SelectNodes => connectSelection(connect(dcfs, maxFinishedProportionUnfinishedNode, commandInvocation.indexListCommandSelection),
        maxFinishedProportionUnfinishedNode)
      case SelectAll => connectSelection(connect(dcfs, maxFinishedProportionUnfinishedNode), maxFinishedProportionUnfinishedNode)
      case ConnectSourceNodesSelectionCommands.Back => dcfs
    }
  }


  private def connect(dcfs: DCFS, maxFinishedProportionUnfinishedNode: Double, selectedSubroots: Option[Seq[Seq[String]]] = None): DCFS = {
    val unfinishedSubroots = Some(dcfs.unfinishedSubroots(sourceForest, targetForest, maxFinishedProportionUnfinishedNode)).map(x => selectedSubroots.map(y => x.filter(z => y
      .exists(z.startsWith(_)))).getOrElse(x)).get
    unfinishedSubroots.foldLeft((dcfs, true)){(x, y) => if(x._2) editRelatedNodes(x._1, y)(withNextCommand = true) else x} match {
      case x if x._2 && unfinishedSubroots.nonEmpty => connect(x._1, maxFinishedProportionUnfinishedNode, selectedSubroots)
      case x => x._1
    }
  }


  private def browse(dcfs: DCFS): DCFS = {
    val without = Seq(TargetNodes)
    val commandInvocation = BrowseCommands.promptUntilParsed(without = without)
    log(commandInvocation)
    commandInvocation.command match {
      case SourceNodes => browseSourceNodes(dcfs)
      case TargetNodes => ???
    }
  }


  private def browseSourceNodes(dcfs: DCFS, sourceNode: Option[Seq[String]] = None): DCFS = {
    val indexToChildrenOrRoots = IndexedCommand.withOneBasedIndexes(sourceNode.map(dcfs.childPaths(sourceForest, _)).getOrElse(dcfs.rootPaths(sourceForest)).toSeq)
    val display_ = (if(indexToChildrenOrRoots.nonEmpty) IndexedCommand.display(indexToChildrenOrRoots.mapValues(display(_, sourceNode.getOrElse(Nil))), sourceNode.map(_ =>
      NodeTypes.Child.name).getOrElse(NodeTypes.Root.name)) else new String) + sourceNode.map(System.lineSeparator() + System.lineSeparator() + display(_))
      .getOrElse(new String)
    val without = Seq(
      (BrowseSourceNodesCommands.GoUp, sourceNode.isEmpty),
      (BrowseSourceNodesCommands.RelatedNodes, sourceNode.isEmpty),
      (BrowseSourceNodesCommands.EditRelatedNodes, sourceNode.isEmpty),
    ).filter(_._2).map(_._1)
    val commandInvocation = BrowseSourceNodesCommands.promptUntilParsed(display_, indexToChildrenOrRoots, without)
    log(commandInvocation)
    commandInvocation.command match {
      case BrowseSourceNodesCommands.GoTo => browseSourceNodes(dcfs, commandInvocation.indexCommandSelection)
      case BrowseSourceNodesCommands.GoUp => browseSourceNodes(dcfs, sourceNode.collect{case x if x.length > 1 => x.init})
      case BrowseSourceNodesCommands.RelatedNodes =>
        println(displayRelatedNodes(dcfs, sourceNode.get))
        IO.promptToPressEnterAndWait()
        IO.clearScreen()
        browseSourceNodes(dcfs, sourceNode)
      case BrowseSourceNodesCommands.EditRelatedNodes => browseSourceNodes(editRelatedNodes(dcfs, sourceNode.get)(withNextCommand = true)._1, sourceNode)
      case BrowseSourceNodesCommands.BackToMainMenu => dcfs
    }
  }


  private def editRelatedNodes(dcfs: DCFS, unfinishedSubroot: Seq[String])(implicit withNextCommand: Boolean): (DCFS, Boolean) = {
    val without = Seq(
      (Descendants, dcfs.children(sourceForest, unfinishedSubroot).isEmpty),
      (Kin, unfinishedSubroot.tail.isEmpty),
      (Next, !withNextCommand)
    ).filter(_._2).map(_._1)
    val commandInvocation = ConnectSourceNodeCommands.promptUntilParsed(display(unfinishedSubroot), without = without)
    log(commandInvocation)
    commandInvocation.command match {
      case RelatedTargetNodes =>
        println(displayRelatedNodes(dcfs, unfinishedSubroot) + System.lineSeparator())
        editRelatedNodes(dcfs, unfinishedSubroot)
      case GoToTargetNodes => editRelatedNodes(goToTargetNodes(dcfs, unfinishedSubroot), unfinishedSubroot)
      case SearchTargetNodes => editRelatedNodes(search(dcfs, unfinishedSubroot, commandInvocation.value(Keyword)), unfinishedSubroot)
      case Descendants =>
        println(displayDescendants(dcfs, sourceForest, unfinishedSubroot, commandInvocation.value(MaxDepth)) + System.lineSeparator())
        editRelatedNodes(dcfs, unfinishedSubroot)
      case Kin =>
        println(displayKin(dcfs, sourceForest, unfinishedSubroot, commandInvocation.value(MaxDepth)) + System.lineSeparator())
        editRelatedNodes(dcfs, unfinishedSubroot)
      case Next =>
        (commandInvocation.value(FinishedValue1To5).map(x => dcfs.withFinishedProportion(sourceForest, unfinishedSubroot, targetForest, finishedProportion(x)))
          .getOrElse(dcfs), true)
      case ConnectSourceNodeCommands.Back => (dcfs, false)
    }
  }


  private def other(dcfs: DCFS): DCFS = {
    val commandInvocation = OtherCommands.promptUntilParsed()
    log(commandInvocation)
    commandInvocation.command match {
      case InitializeProductTaxonomy =>
        val forestLabel = commandInvocation.value(ForestLabel)
        val initializedDcfs = dcfs.withForest(forestLabel).withPaths(forestLabel, ProductTaxonomy.parseFromGreaterThanSeparatedFile(commandInvocation.value(Pathname))
          .labeledForest.paths)
        if(autoSave) save(initializedDcfs)
        other(initializedDcfs)
      case OtherCommands.Back => dcfs
    }
  }


  private def goToTargetNodes(dcfs: DCFS, sourceNode: Seq[String], targetNode: Option[Seq[String]] = None): DCFS = {

    def editName: (DCFS, Seq[String]) = {
      val display_ = s"Rename nodes to change wording and correct spelling only.\nDo not rename nodes to add and remove concepts.\n\n${targetNode.get.last}"
      val commandInvocation = EditNameCommands.promptUntilParsed(display_)
      commandInvocation.command match {
        case Edit => format(commandInvocation.value(PartOfName)) match {case x => (dcfs.withLabel(targetForest, targetNode.get, x), targetNode.get.init :+ x)}
        case EditNameCommands.AbbreviationsForNamingTargetNodes =>
          println(displayAbbreviations)
          editName
        case EditNameCommands.Back => (dcfs, targetNode.get)
      }
    }


    def createNewTargetNodeAndContinue(commandInvocation: CommandInvocation[GoToTargetNodesCommand, Seq[String]]) = {
      val newTargetNode = targetNode.getOrElse(Nil) :+ format(commandInvocation.value(PartOfName))
      val withNewTargetNode = dcfs.withPath(targetForest, newTargetNode) match {case x => if(!commandInvocation.command.parameters.contains(Unrelated) || commandInvocation
        .value(Unrelated)) x else x.withRelationship(sourceForest, sourceNode, targetForest, newTargetNode)}
      goToTargetNodes(withNewTargetNode, sourceNode, targetNode)
    }


    def format(targetNodeName: Seq[String]): String = {
      Some(Display.withSpaces(targetNodeName.headOption.map(x => targetNodeName.tail.+:(abbreviationToTargetNodeNameSubstring.find(_._1.equalsIgnoreCase(x)).map(_._2)
        .getOrElse(x))).getOrElse(targetNodeName))).map(x => if(captializeFirstLetterRelatedNodes) x.capitalize else x).get
    }


    def displayAbbreviations: String = {
      val ellipises = "..."
      Display.table(abbreviationToTargetNodeNameSubstring.toSeq.map(x => Seq(x._1, x._2).map(y => Display.withSpaces(Seq(y, ellipises)))))
    }


    def moveSubtree(indexToChildrenOrRoots: Map[Int, Seq[String]], nNodeToMove: Int, nNodeToMoveInto: Option[Int]): DCFS = {
      object InvalidParameters extends SelfDescribed
      object CannotMoveSubtreeInsideItself extends SelfDescribed
      object NodeWithSameNameAsSubrootAlreadyExistsAtMoveLocation extends SelfDescribed
      val dcfsOrErrorMessage = (indexToChildrenOrRoots.get(nNodeToMove), nNodeToMoveInto.map(indexToChildrenOrRoots.get)) match {
        case x if x._1.isEmpty || x._2.contains(None) => Right(InvalidParameters.getMessage)
        case (Some(x), None) if x.length > 1 && dcfs.roots(targetForest).contains(x.last) => Right(NodeWithSameNameAsSubrootAlreadyExistsAtMoveLocation.getMessage)
        case (Some(x), Some(Some(y))) if y.startsWith(x) => Right(CannotMoveSubtreeInsideItself.getMessage)
        case (Some(x), Some(Some(y))) if dcfs.children(targetForest, y).contains(x.last) => Right(NodeWithSameNameAsSubrootAlreadyExistsAtMoveLocation.getMessage)
        case (Some(x), Some(Some(y))) =>
          val display_ = Display.wordWrap("When moving subtrees non-vertically, any explicit relationships that already exist along the path to the new parent from the " +
            "subtree nodes to nodes from other forests will be removed. This means the move should only be made carefully, because if the subtree were subsequently moved " +
            "again to a path that did not include these new ancestors, the relationships would be lost. Proceed?")
          val commandInvocation = YesNoCommands.promptUntilParsed(display_)
          commandInvocation.command match {
            case Yes => Left(dcfs.withSubtreeMoved(targetForest, x, Some(y)))
            case No => Right(new String)
          }
        case (Some(x), y) => Left(dcfs.withSubtreeMoved(targetForest, x, y.flatten))
      }
      dcfsOrErrorMessage match {
        case Left(x) => x
        case Right(errorMessage) =>
          println(errorMessage)
          goToTargetNodes(dcfs, sourceNode, targetNode)
      }
    }


    def isRelateable(realOrPotentialTargetNode: Seq[String]): Boolean = {
      !(dcfs.relatedNodesOfPath(sourceForest, sourceNode, targetForest).flatten.contains(realOrPotentialTargetNode) || dcfs.relatedNodes(sourceForest, sourceNode,
        targetForest).exists(LabeledForest.isSubOrSuperPathOf(realOrPotentialTargetNode, _)))
    }


    def isNewChildOfRelateable(targetNode: Seq[String]): Boolean = {
      isRelateable(targetNode.:+(Some(dcfs.children(targetForest, targetNode)).filter(_.nonEmpty).map(_.maxBy(_.length)).getOrElse(new String) + 0.toString))
    }


    val displayTargetRoots = "(Target Roots)"
    val displaySourceNodePrefix = "(Source Node: "
    val displaySourceNodeSuffix = ")"


    val indexToChildrenOrRoots = IndexedCommand.withOneBasedIndexes(targetNode.map(dcfs.childPaths(targetForest, _)).getOrElse(dcfs.rootPaths(targetForest)).toSeq)
    val display_ = targetNode.flatMap(x => Some(sourceSubPathAndNonEmptySelectRelatedNodes(dcfs, sourceNode, x)).filter(_.nonEmpty).map(display(_) + System.lineSeparator()))
      .getOrElse(new String) + (if(indexToChildrenOrRoots.nonEmpty) System.lineSeparator() + IndexedCommand.display(indexToChildrenOrRoots.mapValues(display(_, targetNode
      .getOrElse(Nil))), targetNode.map(_ => NodeTypes.Child.name).getOrElse(NodeTypes.Root.name)) + System.lineSeparator() else new String) + System.lineSeparator() + targetNode
      .map(display(_)).getOrElse(displayTargetRoots) + System.lineSeparator() + System.lineSeparator() + displaySourceNodePrefix + display(sourceNode) +
      displaySourceNodeSuffix + System.lineSeparator()


    val without = Seq(
      (GoUp, targetNode.isEmpty),
      (SetAsRelated, targetNode.forall(!isRelateable(_))),
      (RemoveRelatedness, targetNode.forall(!dcfs.relatedNodes(sourceForest, sourceNode, targetForest).contains(_))),
      (CreateNewRelatedTargetRootNode, targetNode.nonEmpty),
      (CreateNewRelatedTargetChildNode, targetNode.forall(!isNewChildOfRelateable(_))),
      (CreateNewTargetChildNode, targetNode.forall(isNewChildOfRelateable)),
      (AbbreviationsForNamingTargetNodes, abbreviationToTargetNodeNameSubstring.isEmpty),
      (EditName, targetNode.isEmpty),
      (MoveChildWithinAnotherChildOrUp, targetNode.forall(dcfs.children(targetForest, _).isEmpty)),
      (MoveRootWithinAnotherRoot, targetNode.nonEmpty || dcfs.roots(targetForest).size <= 1),
      (Delete, targetNode.forall(x => dcfs.relatedNodes(targetForest, x, sourceForest).nonEmpty || dcfs.children(targetForest, x).nonEmpty)),
    ).filter(_._2).map(_._1)


    val commandInvocation = GoToTargetNodesCommands.promptUntilParsed(display_, indexToChildrenOrRoots, without)
    log(commandInvocation)
    commandInvocation.command match {
      case GoTo => goToTargetNodes(dcfs, sourceNode, commandInvocation.indexCommandSelection)
      case GoUp => goToTargetNodes(dcfs, sourceNode, targetNode.collect{case x if x.length > 1 => x.init})
      case SetAsRelated => goToTargetNodes(dcfs.withRelationship(sourceForest, sourceNode, targetForest, targetNode.get), sourceNode, targetNode)
      case RemoveRelatedness => goToTargetNodes(dcfs.withoutRelationship(sourceForest, sourceNode, targetForest, targetNode.get), sourceNode, targetNode)
      case CreateNewRelatedTargetRootNode => createNewTargetNodeAndContinue(commandInvocation)
      case CreateNewRelatedTargetChildNode => createNewTargetNodeAndContinue(commandInvocation)
      case CreateNewTargetChildNode => createNewTargetNodeAndContinue(commandInvocation)
      case AbbreviationsForNamingTargetNodes =>
        println(displayAbbreviations)
        goToTargetNodes(dcfs, sourceNode, targetNode)
      case EditName => editName match {case x => goToTargetNodes(x._1, sourceNode, Some(x._2))}
      case MoveChildWithinAnotherChildOrUp =>
        goToTargetNodes(moveSubtree(indexToChildrenOrRoots, commandInvocation.value(NumberOfChildToMove), commandInvocation.value(NumberOfChildToMoveInto)), sourceNode,
          targetNode)
      case MoveRootWithinAnotherRoot =>
        goToTargetNodes(moveSubtree(indexToChildrenOrRoots, commandInvocation.value(NumberOfRootToMove), Some(commandInvocation.value(NumberOfRootToMoveInto))), sourceNode,
          targetNode)
      case Delete => goToTargetNodes(dcfs.withoutSubtree(targetForest, targetNode.get), sourceNode, targetNode.collect{case x if x.length > 1 => x.init})
      case GoToTargetNodesCommands.Back =>
        if(autoSave) save(dcfs)
        dcfs
    }

  }


  private def search(dcfs: DCFS, sourceForestNode: Seq[String], query: Seq[String]): DCFS = {
    val keywordsSeparator = " "
    val header = "Search Result"
    val maxNResults = 100
    val indexToResult = IndexedCommand.withOneBasedIndexes(dcfs.resultPathToNormalizedScore(targetForest, query.mkString(keywordsSeparator),
      maxNResults).toSeq.sortBy(-_._2).map(_._1))
    val display_ = IndexedCommand.display(indexToResult.mapValues(display(_)), header, reverse = true)
    val commandInvocation = SearchResultCommands.promptUntilParsed(display_, indexToResult)
    log(commandInvocation)
    commandInvocation.command match {
      case GoToResultNumber => goToTargetNodes(dcfs, sourceForestNode, commandInvocation.indexCommandSelection)
      case SearchResultCommands.Search => search(dcfs, sourceForestNode, commandInvocation.value(Keyword))
      case SearchResultCommands.Back => dcfs
    }
  }


  private def displayRelatedNodes(dcfs: DCFS, sourceForestNode: Seq[String]): String = {
    val indent = "  "
    val noRelatedNodesSymbol = "()"
    display(LabeledForest.subPaths(sourceForestNode).zip(dcfs.relatedNodesOfPath(sourceForest, sourceForestNode, targetForest)))
  }


  private def sourceSubPathAndNonEmptySelectRelatedNodes(dcfs: DCFS, sourceNode: Seq[String], targetNodeToDisplayOnlySubOrSuperPathsOf: Seq[String]
                                                        ): Seq[(Seq[String], Set[Seq[String]])] = {
    LabeledForest.subPaths(sourceNode).zip(dcfs.relatedNodesOfPath(sourceForest, sourceNode, targetForest).map(_.filter(LabeledForest.isSubOrSuperPathOf(_,
      targetNodeToDisplayOnlySubOrSuperPathsOf)))).filter(_._2.nonEmpty)
  }


  private def display(sourceSubPathAndRelatedNodes: Seq[(Seq[String], Set[Seq[String]])]): String = {
    val indent = "  "
    val noRelatedNodesSymbol = "()"
    Display.table(sourceSubPathAndRelatedNodes.map(x => Seq(display(x._1)) ++ Some(x._2.map(display(_))).filter(_.nonEmpty).getOrElse(Seq(noRelatedNodesSymbol)).map(indent +
      _)).flatMap(_.map(Seq(_))))
  }


  private def displayDescendants(dcfs: DCFS, forestLabel: String, node: Seq[String], maxDepth: Int, header: Option[String] = None): String = {
    Display.table(dcfs.pathsSubtree(forestLabel, node).-(node).filter(_.length - node.length <= maxDepth).toSeq.sortWith((x, y) => x.length < y.length && y
      .startsWith(x) || x.zip(y).find(z => z._1 != z._2).exists(z => z._1.compare(z._2) < 0)).map(x => Seq(display(x, node))), header.map(Seq(_)).getOrElse(Nil))
  }


  private def displayKin(dcfs: DCFS, forestLabel: String, node: Seq[String], maxDepth: Int): String = {
    val depth = Math.min(maxDepth, node.length - 1)
    val subroot = node.dropRight(depth)
    displayDescendants(dcfs, sourceForest, subroot, depth, Some(display(subroot, subroot.init, replaceExcludedSubpathWithEllipses = true)))
  }


  private def finishedProportion(finishedValue: Int): Double = {
    (finishedValue - finishedValues.min).toDouble / (finishedValues.max - finishedValues.min).toDouble
  }


  private def display(path: Seq[String], withoutSubpath: Seq[String] = Nil, withForestType: Option[ForestType] = None,
                      replaceExcludedSubpathWithEllipses: Boolean = false): String = {
    val separator = " -> "
    val ellipses = "..."
    val forestTypeToLabel = Map(
      ForestTypes.Source -> "(s)",
      ForestTypes.Target -> "(t)"
    )
    val pathString = Display.withSpacedArrows(withoutSubpath.inits.toList.find(path.startsWith(_)).get match {
      case x if x.nonEmpty && replaceExcludedSubpathWithEllipses => path.drop(x.length).+:(ellipses)
      case x if x.nonEmpty => path.drop(x.length)
      case x => path
    })
    withForestType.map(x => Display.withSpaces(Seq(forestTypeToLabel(x), pathString))).getOrElse(pathString)
  }


  private def save(dcfs: DCFS): Unit = {
    //todo error-handling
    IO.write(persistencePathname, jsonFormat.toJson(dcfs.connectedForestsAndRelatedNodesToFinishedProportion).toString())
  }


  private def log[T <: Command, U](commandInvocation: CommandInvocation[T, U]): Unit = {

    def display(value: U): String = {
      value match {
        case x: Seq[_] => Display.withCommaSpaces(x.map(_.toString))
        case x => x.toString
      }
    }


    logPath.foreach{ path =>
      val zonedDateTime = ZonedDateTime.now(ZoneOffset.UTC).truncatedTo(ChronoUnit.SECONDS)
      val command = Display.withSpaces(Seq(Some(commandInvocation.command.name), commandInvocation.indexCommandSelection.map(display).orElse(commandInvocation
        .indexListCommandSelection.map(x => Display.withSemicolonSpaces(x.map(display))))).collect{case Some(x) => x})
      //todo error-handling
      IO.append(Paths.get(path, zonedDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE)).toString, Display.withTabs(Seq(zonedDateTime.format(DateTimeFormatter
          .ISO_OFFSET_DATE_TIME), command, Display.withSpaces(commandInvocation.arguments))))
    }

  }


  private object NodeTypes {
    object Root extends SelfNamed
    object Child extends SelfNamed
  }


  private[consoleApplication] object ForestTypes extends Enumerated {

    override type T = ForestType
    sealed abstract class ForestType extends SelfNamed

    object Source extends ForestType
    object Target extends ForestType


    override protected val enumeratedTypes: ForestTypes.EnumeratedTypes = EnumeratedTypes(u.typeOf[ForestTypes.type], classOf[ForestType])

  }

}
