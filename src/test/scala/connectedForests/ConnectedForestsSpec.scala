package connectedForests

import connectedForests.ConnectedForestsSpec.ConnectedForestsTestData
import connectedForests.LabeledForestSpec.LabeledForestTestData
import org.scalatest.FunSpec

class ConnectedForestsSpec extends FunSpec with ConnectedForestsTestData {

  describe("ConnectedForests") {

    describe("children"){
      it("should return the node's children if the node exists, and throw an exception otherwise"){
        pathTo__parentLabel_childrenLabels.foreach(x => assert(connectedForests.children(forest1Label, x._1) == x._2._2))
        nonExistentPaths.foreach(x => assertThrows[Exception](connectedForests.children(forest1Label, x).isEmpty))
      }
    }


    describe("forestLabels"){
      it("should return the forest labels"){
        assert(ConnectedForests().forestLabels.isEmpty)
        assert(connectedForests.forestLabels == Set(forest1Label, forest2Label))
      }
    }


    describe("id"){
      //todo
    }


    describe("paths"){
      it("should return the paths of all nodes in the forest"){
        assert(connectedForests.paths(forest1Label) == pathTo__parentLabel_childrenLabels.keys)
      }
    }


    describe("pathsSubtree"){
      //todo
    }


    describe("relatedNodes"){
      it("should return the nodes marked as related to a node that were not since marked unrelated to the node or deleted"){
        paths.foreach(x => connectedForests.relatedNodes(forest1Label, x, forest2Label).isEmpty)
        paths.foreach(x => connectedForests.relatedNodes(forest2Label, x, forest1Label).isEmpty)
        val completelyConnectedForests = paths.flatMap(x => paths.map((x, _))).foldLeft(connectedForests)((x, y) => x.withRelationship(forest1Label, y._1, forest2Label, y._2))
        paths.foreach(x => assert(completelyConnectedForests.relatedNodes(forest1Label, x, forest2Label) == paths))
        paths.foreach(x => assert(completelyConnectedForests.relatedNodes(forest2Label, x, forest1Label) == paths))
        paths.flatMap(x => paths.map((x, _))).foreach(x => assert(completelyConnectedForests.withoutRelationship(forest1Label, x._1, forest2Label, x._2)
          .relatedNodes(forest1Label, x._1, forest2Label) == paths.toSet.-(x._2)))
        paths.flatMap(x => paths.map((x, _))).foreach(x => assert(completelyConnectedForests.withoutRelationship(forest1Label, x._1, forest2Label, x._2)
          .relatedNodes(forest2Label, x._2, forest1Label) == paths.toSet.-(x._1)))
        paths.flatMap(x => paths.map((x, _))).foreach(x => assert(completelyConnectedForests.withoutSubtree(forest2Label, x._2)
          .relatedNodes(forest1Label, x._1, forest2Label) == paths.toSet -- paths.filter(_.startsWith(x._2))))
        paths.foreach(x => assert(completelyConnectedForests.withoutForest(forest2Label).withForest(forest2Label).withPaths(forest2Label, completelyConnectedForests
          .paths(forest2Label)).relatedNodes(forest1Label, x, forest2Label).isEmpty))
      }
    }


    describe("roots"){
      it("should return the root nodes' labels"){
        assert(connectedForests.roots(forest1Label) == rootLabels)
      }
    }


    describe("sortedDistanceToNonAncestralOrDescendentNodeAndRelatedNodes"){
      //todo
    }


    describe("withForest"){
      it("should ensure the connected forests contains a forest with that label"){
        assert(connectedForests.withForest(nonExistentForestLabel).forestLabels.contains(nonExistentForestLabel))
        assert(connectedForests.withForest(forest1Label).forestLabels.contains(forest1Label))
      }
    }


    describe("withLabel"){
      it("should relabel a node if both the node exists and a sibling with the label does not exist"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          val relabeledCF = connectedForests.withLabel(forest1Label, path, nonExistentLabel)
          relabeledCF.children(forest1Label, path.slice(0, path.length - 1).:+(nonExistentLabel))
          assertThrows[Exception](relabeledCF.children(forest1Label, path))
        }
      }
      it("should transfer a node's children to the sibling and remove the node if both the node and a sibling with the label exist"){
        pathTo__parentLabel_childrenLabels.foreach { path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          pathTo__parentLabel_childrenLabels.keys.find(x => x != path && x.slice(0, x.length - 1) == path.slice(0, path.length - 1)).foreach { siblingPath =>
            val relabeledCF = connectedForests.withLabel(forest1Label, path, siblingPath.last)
            val childrenLabelsOriginalPath = pathTo__parentLabel_childrenLabels(path)._2
            val childrenLabelsOriginialSiblingPath = pathTo__parentLabel_childrenLabels(siblingPath)._2
            assert(relabeledCF.children(forest1Label, siblingPath) == childrenLabelsOriginialSiblingPath ++ childrenLabelsOriginalPath)
            assertThrows[Exception](relabeledCF.children(forest1Label, path))
          }
        }
      }
      it("should throw an exception if the node does not exist"){
        nonExistentPaths.foreach(x => assertThrows[Exception](connectedForests.withLabel(forest1Label, x, nonExistentLabel)))
      }
    }


    describe("withPath"){
      it("should return a copy of the forest with the path and its subpaths"){
        paths.foreach { path =>
          val newLfPaths = connectedForests.withForest(nonExistentForestLabel).withPath(nonExistentForestLabel, path).paths(nonExistentForestLabel)
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == subpaths.toSet)
        }
        nonExistentPaths.foreach { path =>
          val newLfPaths = connectedForests.withPath(forest1Label, path).paths(forest1Label)
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == connectedForests.paths(forest1Label).++(subpaths.toSet))
        }
        paths.foreach(x => assert(connectedForests.withPath(forest1Label, x).paths(forest1Label) == connectedForests.paths(forest1Label)))
      }
    }


    describe("withPaths"){
      it("should return a copy of the forest with the paths and their subpaths"){
        assert(connectedForests.withForest(nonExistentForestLabel).withPaths(nonExistentForestLabel, pathsToLeaves).paths(nonExistentForestLabel) == connectedForests
          .paths(forest1Label))
        assert(connectedForests.withForest(nonExistentForestLabel).withPaths(nonExistentForestLabel, paths).paths(nonExistentForestLabel) == connectedForests.paths(forest1Label))
        assert(connectedForests.withPaths(forest1Label, pathsToLeaves).paths(forest1Label) == connectedForests.paths(forest1Label))
        assert(connectedForests.withPaths(forest1Label, paths).paths(forest1Label) == connectedForests.paths(forest1Label))
      }
    }


    describe("withRelationship"){
      it("should symmetically associate the two nodes"){
        val completelyConnectedForests = paths.flatMap(x => paths.map((x, _))).foldLeft(connectedForests)((x, y) => x.withRelationship(forest1Label, y._1, forest2Label, y._2))
        paths.foreach(x => assert(completelyConnectedForests.relatedNodes(forest1Label, x, forest2Label) == paths))
        paths.foreach(x => assert(completelyConnectedForests.relatedNodes(forest2Label, x, forest1Label) == paths))
      }
    }


    describe("withoutRelationship"){
      it("should symmetrically disassociate the two nodes") {
        val completelyConnectedForests = paths.flatMap(x => paths.map((x, _))).foldLeft(connectedForests)((x, y) => x.withRelationship(forest1Label, y._1, forest2Label, y._2))
        paths.flatMap(x => paths.map((x, _))).foreach(x => assert(completelyConnectedForests.withoutRelationship(forest1Label, x._1, forest2Label, x._2)
          .relatedNodes(forest1Label, x._1, forest2Label) == paths.toSet.-(x._2)))
        paths.flatMap(x => paths.map((x, _))).foreach(x => assert(completelyConnectedForests.withoutRelationship(forest1Label, x._1, forest2Label, x._2)
          .relatedNodes(forest2Label, x._2, forest1Label) == paths.toSet.-(x._1)))
      }
    }


    describe("withoutForest"){
      it("should remove the forest and its relationships"){
        assert(connectedForests.withoutForest(forest2Label).forestLabels == connectedForests.forestLabels - forest2Label)
        val completelyConnectedForests = paths.flatMap(x => paths.map((x, _))).foldLeft(connectedForests)((x, y) => x.withRelationship(forest1Label, y._1, forest2Label, y._2))
        paths.foreach(x => assert(completelyConnectedForests.withoutForest(forest2Label).withForest(forest2Label).withPaths(forest2Label, completelyConnectedForests
          .paths(forest2Label)).relatedNodes(forest1Label, x, forest2Label).isEmpty))
      }
    }


    describe("withoutSubtree"){
      it("should return a copy of the forest without the subtree and any previous relationships involving it"){
        paths.foreach{path =>
          val pathsSubtree = paths.filter(_.startsWith(path))
          val connectedForestsWithSubtreeRelationships = pathsSubtree.foldLeft(connectedForests)((x, y) => x.withRelationship(forest1Label, y, forest2Label, y))
          val connectedForestsWithoutSubtree = connectedForestsWithSubtreeRelationships.withoutSubtree(forest1Label, path)
          assert(connectedForestsWithoutSubtree.paths(forest1Label) == paths.toSet.--(pathsSubtree))
          pathsSubtree.foreach(x => assert(connectedForestsWithoutSubtree.relatedNodes(forest2Label, x, forest1Label).isEmpty))
        }
      }
    }


    describe("withSubtreeMoved"){
      it("should return a connected forests object with the applicable forest the same as if withSubtreeMoved had been performed individually on it"){
        //todo
      }
      it("should return a connected forests object without any relationships in the moved subtree that already exist along the path to the new parent"){
        //todo
      }
    }

  }

}

object ConnectedForestsSpec {

  trait ConnectedForestsTestData extends LabeledForestTestData {
    val forest1Label = 0
    val forest2Label = 1
    val nonExistentForestLabel: Int = -1
    val connectedForests: ConnectedForests[Int, Int] = ConnectedForests().withForest(forest1Label).withPaths(forest1Label, pathsToLeaves).withForest(forest2Label)
      .withPaths(forest2Label, pathsToLeaves)
  }

}
