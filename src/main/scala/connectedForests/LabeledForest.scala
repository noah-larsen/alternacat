package connectedForests

import connectedForests.LabeledForest.Fields.{Field, Id}
import connectedForests.LabeledForest.{Fields, LabeledTreeNode}
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.document.Field.Store
import org.apache.lucene.document.{Document, TextField}
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig}
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.{IndexSearcher, MatchNoDocsQuery, Query}
import org.apache.lucene.store.RAMDirectory
import utils.enumerated.{Enumerated, SelfNamed}

import scala.util.Try

case class LabeledForest[N] private(
                                     private val idToNode: Map[Long, LabeledTreeNode[N]],
                                     private val labelToRootId: Map[N, Long],
                                     private val ramDirectory: RAMDirectory
                                   ) {

  def this(){
    this(Map(), Map(), new RAMDirectory())
  }


  def children(path: Seq[N]): Set[N] = {
    idToNode(id(path)).labelToChildId.keySet
  }


  def distance(path1: Seq[N], path2: Seq[N]): Option[Int] = {
    def lowestCommonAncestor(path1: Seq[N], path2: Seq[N]): Seq[N] = path1.zip(path2).takeWhile(x => x._1 == x._2).map(_._1)
    Some(lowestCommonAncestor(path1, path2).length).filter(_ > 0).map(x => path1.length - x + path2.length - x)
  }


  def id(path: Seq[N]): Long = {
    path.tail.foldLeft(labelToRootId(path.head))((x, y) => idToNode(x).labelToChildId(y))
  }


  def idsSubtree(path: Seq[N]): Set[Long] = {
    idsSubtree(id(path))
  }


  def nonAncestorDescendantNodesSameTree(path: Seq[N]): Set[Seq[N]] = {
    paths -- pathsSubtree(path) -- LabeledForest.subPaths(path)
  }


  def path(id: Long): Seq[N] = {
    val node = idToNode(id)
    node.parentId.map(y => path(y).:+(node.label)).getOrElse(Seq(node.label))
  }


  def pathToId: Map[Seq[N], Long] = {
    paths.map(x => (x, id(x))).toMap
  }


  def paths: Set[Seq[N]] = {
    idToNode.keys.map(path).toSet
  }


  def pathsSubtree(path: Seq[N]): Set[Seq[N]] = {
    idsSubtree(path).map(this.path)
  }


  def roots: Set[N] = {
    labelToRootId.keySet
  }


  def withLabel(path: Seq[N], label: N): LabeledForest[N] = {

    def copyChildren(idToNode: Map[Long, LabeledTreeNode[N]], from: Long, to: Long): Map[Long, LabeledTreeNode[N]] = {
      idToNode.+(to -> idToNode(to).addChildren(idToNode(from).labelToChildId))
    }


    def relabelIdToNode(idToNode: Map[Long, LabeledTreeNode[N]], id: Long, label: N): Map[Long, LabeledTreeNode[N]] = {
      val withParentUpdated = idToNode(id).parentId
        .map(x => idToNode.+(x -> idToNode(x).removeChild(idToNode(id).label).addChild(label, id)))
        .getOrElse(idToNode)
      withParentUpdated.+(id -> idToNode(id).withLabel(label))
    }


    def relabelLabelToRootId(labelToRootId: Map[N, Long], id: Long, label: N): Map[N, Long] = {
      labelToRootId.find(_._2 == id)
        .map(x => labelToRootId.-(x._1).+(label -> id))
        .getOrElse(labelToRootId)
    }


    def siblingId(path: Seq[N], label: N): Long = {
      id(path.slice(0, path.length - 1).:+(label))
    }

    val idNodeToRelabel = id(path)
    Try(siblingId(path, label))
      .map(x => new LabeledForest[N](
        LabeledForest.withoutNode(copyChildren(idToNode, idNodeToRelabel, x), idNodeToRelabel),
        LabeledForest.withoutNode(labelToRootId, idToNode(idNodeToRelabel)),
        ramDirectory
      ))
      .getOrElse(new LabeledForest[N](relabelIdToNode(idToNode, idNodeToRelabel, label), relabelLabelToRootId(labelToRootId, idNodeToRelabel, label), ramDirectory))

  }


  def withPath(path: Seq[N]): LabeledForest[N] = {
    withPaths(Seq(path))
  }


  def withPaths(paths: Iterable[Seq[N]]): LabeledForest[N] = {
    withPaths(paths, Map())
  }


  def withoutSubtree(path: Seq[N]): LabeledForest[N] = {
    val rootId = id(path)
    new LabeledForest[N](
      LabeledForest.withoutNode(idToNode, rootId).--(idsSubtree(rootId)),
      LabeledForest.withoutNode(labelToRootId, idToNode(rootId)),
      ramDirectory
    )
  }


  private def idsSubtree(rootId: Long): Set[Long] = {
    idToNode.get(rootId).map(_.labelToChildId.values.foldLeft(Set(rootId))((x, y) => x.++(idsSubtree(y)))).getOrElse(Set())
  }


  private def withPaths(paths: Iterable[Seq[N]], subPathToId: Map[Seq[N], Long]): LabeledForest[N] = {

    def subpaths(path: Seq[N]): Seq[Seq[N]] = {
      path.zipWithIndex.map(x => path.take(x._2 + 1))
    }


    def nextId(labeledForest: LabeledForest[N]): Long = {
      Some(labeledForest.idToNode.keys).filter(_.nonEmpty)
        .map(_.max + 1)
        .getOrElse(0)
    }


    val labeledForest = paths.foldLeft(this) { (labeledForest, path) =>
      subpaths(path).foldLeft((labeledForest, None: Option[Long])) { (labeledForest_parentId, subPath) =>
        val labeledForest = labeledForest_parentId._1
        val parentId = labeledForest_parentId._2
        val id = parentId.map(labeledForest.idToNode(_).labelToChildId).getOrElse(labeledForest.labelToRootId).get(subPath.last)
        val subPathId = subPathToId.getOrElse(subPath, nextId(labeledForest))
        val labeledForestWithSubpath = id.map(_ => labeledForest).getOrElse(
          new LabeledForest[N](
            parentId.map(x => labeledForest.idToNode.+(x -> labeledForest.idToNode(x).addChild(subPath.last, subPathId))).getOrElse(labeledForest.idToNode)
              .+(subPathId -> LabeledTreeNode(subPath.last, parentId, Map())),
            parentId.map(_ => labeledForest.labelToRootId).getOrElse(labeledForest.labelToRootId.+(subPath.last -> id.getOrElse(subPathId))),
            ramDirectory
          )
        )
        (labeledForestWithSubpath, id.orElse(Some(subPathId)))
      }._1
    }


    labeledForest.addPathsToIndex(paths)
    labeledForest

  }


  private val analyzer = new StandardAnalyzer()
  private val pathStringSeparator = "; "


  private def addPathsToIndex(paths: Iterable[Seq[N]]): Unit = {
    val indexWriter = new IndexWriter(ramDirectory, new IndexWriterConfig(analyzer).setOpenMode(OpenMode.CREATE_OR_APPEND))
    paths.foreach { path =>
      val document = new Document()
      document.add(new TextField(Fields.Id.name, id(path).toString, Store.YES))
      document.add(new TextField(Fields.Path.name, path.mkString(pathStringSeparator), Store.NO))
      indexWriter.addDocument(document)
    }
    indexWriter.close()
  }


  def resultPathToNormalizedScore(query: String, maxNResults: Integer, fields: Seq[Field] = Fields.values): Map[Seq[N], Double] = {
    val directoryReader = DirectoryReader.open(ramDirectory)
    try {
      val indexSearcher = new IndexSearcher(directoryReader)
      val topDocs = indexSearcher.search(parse(query, fields), maxNResults)
      val resultPathToScore = topDocs.scoreDocs.map(x => (path(indexSearcher.doc(x.doc).getField(Id.name).stringValue().toLong), x.score.toDouble)).toMap
      lazy val maxScore = resultPathToScore.values.max
      resultPathToScore.mapValues(x => if (maxScore > 0) x / maxScore else x)
    } finally directoryReader.close()
  }


  private def parse(query: String, fields: Seq[Field]): Query = {
    val remove = Seq("~", "`", "!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "+", "=", "{", "}", "[", "]", "|", "\\", ":", ";", "\"", "<", ",", ">", ".", "?", "/")
    val removeAllFromBeginningTerm = '-'
    val defineTermForFieldOperator = ":"
    val separator = " "
    val whitespaceRe = "\\s+"
    val orOperator = "OR"
    val queryTerms = remove.foldLeft(query)((x, y) => x.replace(y, new String)).toLowerCase.split(whitespaceRe).distinct.map(_.dropWhile(_ == removeAllFromBeginningTerm))
      .filter(_ != new String)
    queryTerms.length match {
      case 0 => new MatchNoDocsQuery
      case _ =>
        val fieldDefinedTerms = queryTerms.flatMap(x => fields.map(_.name + defineTermForFieldOperator + x))
        new QueryParser(new String, analyzer).parse(fieldDefinedTerms.mkString(separator + orOperator + separator))
    }
  }

}

object LabeledForest {

  def apply[N](paths: Iterable[Seq[N]] = Iterable[Seq[N]]()): LabeledForest[N] = {
    new LabeledForest().withPaths(paths)
  }


  def apply[N](pathToId: Map[Seq[N], Long]): LabeledForest[N] = {
    LabeledForest[N]().withPaths(pathToId.keys, pathToId)
  }


  def subOrSuperPathsOf[N](path1: Seq[N], path2: Seq[N]): Boolean = {
    path1.startsWith(path2) || path2.startsWith(path1)
  }


  def subPaths[N](path: Seq[N]): Seq[Seq[N]] = {
    path.inits.toSeq.reverse.tail
  }


  object Fields extends Enumerated {

    override type T = Field
    sealed trait Field extends SelfNamed

    object Id extends Field
    object Path extends Field


    override protected val enumeratedTypes = EnumeratedTypes(u.typeOf[Fields.type], classOf[Field])

  }


  private def withoutNode[N](idToNode: Map[Long, LabeledTreeNode[N]], id: Long): Map[Long, LabeledTreeNode[N]] = {
    idToNode(id).parentId.map(x => idToNode.+(x -> idToNode(x).removeChild(idToNode(id).label))).getOrElse(idToNode).-(id)
  }


  private def withoutNode[N](labelToRootId: Map[N, Long], node: LabeledTreeNode[N]): Map[N, Long] = {
    node.parentId.map(_ => labelToRootId).getOrElse(labelToRootId.-(node.label))
  }


  private[connectedForests] case class LabeledTreeNode[N](
                                         label: N,
                                         parentId: Option[Long],
                                         labelToChildId: Map[N, Long]
                                       ){

    def addChild(label: N, id: Long): LabeledTreeNode[N] = {
      LabeledTreeNode(this.label, parentId, labelToChildId.+(label -> id))
    }


    def addChildren(labelToChildId: Map[N, Long]): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, this.labelToChildId.++(labelToChildId))
    }


    def removeChild(label: N): LabeledTreeNode[N] = {
      LabeledTreeNode(this.label, parentId, labelToChildId.-(label))
    }


    def removeChildren(labels: Set[N]): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, labelToChildId.--(labels))
    }


    def withLabel(label: N): LabeledTreeNode[N] = {
      LabeledTreeNode(label, parentId, labelToChildId)
    }

  }

}
