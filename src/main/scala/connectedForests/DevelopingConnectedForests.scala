package connectedForests

import connectedForests.ConnectedForests.RelatedNodesKey

case class DevelopingConnectedForests[F, N] private (
                                                      private val connectedForests: ConnectedForests[F, N],
                                                      private val relatedNodesKeyToFinishedProportion: Map[RelatedNodesKey[F], Double]
                                                    ) extends AbstractConnectedForests[F, N] {

  def finishedProportion(fromForestLabel: F, path: Seq[N], toForestLabel: F): Double = {
    relatedNodesKeyToFinishedProportion.getOrElse(relatedNodesKey(fromForestLabel, path, toForestLabel), 0)
  }


  def connectedForestsAndRelatedNodesToFinishedProportion: (ConnectedForests[F, N], Map[(F, Long, F), Double]) = {
    (connectedForests, relatedNodesKeyToFinishedProportion.map(x => ((x._1.fromForestLabel, x._1.fromForestNodeId, x._1.toForestLabel), x._2)))
  }


  def unfinishedSubroots(fromForestLabel: F, toForestLabel: F, maxFinishedProportionUnfinishedNode: Double,
                         sortBy: (ConnectedForests[F, N], F) => Seq[N] => Seq[Int] = DevelopingConnectedForests.SortByFs.breadthFirstLargestSubtree): Seq[Seq[N]] = {

    def unfinishedSubroots(subrootPaths: Iterable[Seq[N]] = connectedForests.roots(fromForestLabel).map(Seq(_))): Iterable[Seq[N]] = {
      subrootPaths.flatMap{
        case x if finishedProportion(fromForestLabel, x, toForestLabel) > maxFinishedProportionUnfinishedNode && connectedForests.children(fromForestLabel, x).isEmpty => Seq()
        case x if finishedProportion(fromForestLabel, x, toForestLabel) > maxFinishedProportionUnfinishedNode => unfinishedSubroots(connectedForests.children(fromForestLabel,
          x).map(x :+ _))
        case x => Seq(x)
      }
    }


    unfinishedSubroots().toSeq.sortBy(sortBy(connectedForests, fromForestLabel))(Ordering.Implicits.seqDerivedOrdering)

  }


  def withFinishedProportion(fromForestLabel: F, path: Seq[N], toForestLabel: F, finishedProportion: Double): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests, relatedNodesKeyToFinishedProportion.+((relatedNodesKey(fromForestLabel, path, toForestLabel), finishedProportion)))
  }


  override def children(forestLabel: F, path: Seq[N]): Set[N] = {
    connectedForests.children(forestLabel, path)
  }


  override def distance(forestLabel: F, path1: Seq[N], path2: Seq[N]): Option[Int] = {
    connectedForests.distance(forestLabel, path1, path2)
  }


  override def forestLabels: Set[F] = {
    connectedForests.forestLabels
  }


  override def nonAncestorDescendantNodesSameTree(forestLabel: F, path: Seq[N]): Set[Seq[N]] = {
    connectedForests.nonAncestorDescendantNodesSameTree(forestLabel, path)
  }


  override def paths(forestLabel: F): Set[Seq[N]] = {
    connectedForests.paths(forestLabel)
  }


  override def pathsSubtree(forestLabel: F, path: Seq[N]): Set[Seq[N]] = {
    connectedForests.pathsSubtree(forestLabel, path)
  }


  override def relatedNodes(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Set[Seq[N]] = {
    connectedForests.relatedNodes(fromForestLabel, fromForestPath, toForestLabel)
  }


  override def relatedNodesOfPath(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Seq[Set[Seq[N]]] = {
    connectedForests.relatedNodesOfPath(fromForestLabel, fromForestPath, toForestLabel)
  }


  override def resultPathToNormalizedScore(forestLabel: F, query: String, maxNResults: Integer): Map[Seq[N], Double] = {
    connectedForests.resultPathToNormalizedScore(forestLabel, query, maxNResults)
  }


  override def roots(forestLabel: F): Set[N] = {
    connectedForests.roots(forestLabel)
  }


  override def unrelatedNodeToMinDistanceFromRelatedNonAncestorDescendantSourceNode(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Map[Seq[N], Int] = {
    connectedForests.unrelatedNodeToMinDistanceFromRelatedNonAncestorDescendantSourceNode(fromForestLabel, fromForestPath, toForestLabel)
  }


  override def unrelatedNodeToMinDistanceFromNonAncestorDescendantRelatedTargetNode(fromForestLabel: F, fromForestPath: Seq[N], toForestLabel: F): Map[Seq[N], Int] = {
    connectedForests.unrelatedNodeToMinDistanceFromNonAncestorDescendantRelatedTargetNode(fromForestLabel, fromForestPath, toForestLabel)
  }


  override def withForest(label: F): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withForest(label), relatedNodesKeyToFinishedProportion)
  }


  override def withLabel(forestLabel: F, path: Seq[N], label: N): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withLabel(forestLabel, path, label), relatedNodesKeyToFinishedProportion)
  }


  override def withPath(forestLabel: F, path: Seq[N]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withPath(forestLabel, path), relatedNodesKeyToFinishedProportion)
  }


  override def withPaths(forestLabel: F, paths: Iterable[Seq[N]]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withPaths(forestLabel, paths), relatedNodesKeyToFinishedProportion)
  }


  override def withRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withRelationship(forest1Label, forest1Path, forest2Label, forest2Path), relatedNodesKeyToFinishedProportion)
  }


  override def withoutRelationship(forest1Label: F, forest1Path: Seq[N], forest2Label: F, forest2Path: Seq[N]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withoutRelationship(forest1Label, forest1Path, forest2Label, forest2Path), relatedNodesKeyToFinishedProportion)
  }


  override def withoutForest(forestLabel: F): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withoutForest(forestLabel), relatedNodesKeyToFinishedProportion.filterKeys(x => !Seq(x.fromForestLabel, x.toForestLabel)
      .contains(forestLabel)))
  }


  override def withoutSubtree(forestLabel: F, path: Seq[N]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withoutSubtree(forestLabel, path), relatedNodesKeyToFinishedProportion.filterKeys(x => connectedForests.pathsSubtree(forestLabel,
      path).exists(y => x.fromForestLabel == forestLabel && x.fromForestNodeId == connectedForests.id(forestLabel, y))))
  }


  override def withSubtreeMoved(forestLabel: F, path: Seq[N], pathNewParent: Option[Seq[N]]): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForests.withSubtreeMoved(forestLabel, path, pathNewParent), relatedNodesKeyToFinishedProportion)
  }


  override protected type Self = DevelopingConnectedForests[F, N]


  private def relatedNodesKey(fromForestLabel: F, path: Seq[N], toForestLabel: F): RelatedNodesKey[F] = {
    RelatedNodesKey(fromForestLabel, connectedForests.id(fromForestLabel, path), toForestLabel)
  }

}

object DevelopingConnectedForests {

  def apply[F, N](
                   connectedForests: ConnectedForests[F, N] = ConnectedForests[F, N](),
                   relatedNodesKeyToFinishedProportion: Map[RelatedNodesKey[F], Double] = Map[RelatedNodesKey[F], Double]()
                 ): DevelopingConnectedForests[F, N] = {
    new DevelopingConnectedForests(connectedForests, relatedNodesKeyToFinishedProportion)
  }


  def apply[F, N](connectedForestsAndRelatedNodesToFinishedProportion: (ConnectedForests[F, N], Map[(F, Long, F), Double])): DevelopingConnectedForests[F, N] = {
    DevelopingConnectedForests(connectedForestsAndRelatedNodesToFinishedProportion._1, connectedForestsAndRelatedNodesToFinishedProportion._2.map(x => (RelatedNodesKey(
      x._1._1, x._1._2, x._1._3), x._2)))
  }


  object SortByFs {

    def breadthFirstLargestSubtree[F, N](connectedForests: ConnectedForests[F, N], fromForestLabel: F)(path: Seq[N]): Seq[Int] = {
      Seq(path.length, -connectedForests.pathsSubtree(fromForestLabel, path).size)
    }

  }

}