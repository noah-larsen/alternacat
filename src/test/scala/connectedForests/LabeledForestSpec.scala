package connectedForests

import connectedForests.LabeledForestSpec.LabeledForestTestData
import org.scalatest.FunSpec

class LabeledForestSpec extends FunSpec with LabeledForestTestData {

  describe("LabeledForest"){

    describe("children"){
      it("should return the node's children if the node exists, and throw an exception otherwise"){
        pathTo__parentLabel_childrenLabels.foreach(x => assert(labeledForest.children(x._1) == x._2._2))
        nonExistentPaths.foreach(x => assertThrows[Exception](labeledForest.children(x).isEmpty))
      }
    }


    describe("distance"){
      val rootToPaths = paths.groupBy(_.head)
      it("should return 0 for any node to itself"){
        paths.foreach(x => assert(labeledForest.distance(x, x).contains(0)))
      }
      it("should return None for any node to a node with a different root"){
        paths.foreach(x => rootToPaths.filter(_._1 != x.head).values.flatten.foreach(y => assert(labeledForest.distance(x, y).isEmpty)))
      }
      it("should return Some(x) for any node to a node with the same root"){
        paths.foreach(x => rootToPaths.filter(_._1 == x.head).values.flatten.foreach(y => assert(labeledForest.distance(x, y).isDefined)))
      }
      it("should return the number of nodes in the path - 1 for any leaf to its root"){
        pathsToLeaves.foreach(x => assert(labeledForest.distance(x, Seq(x.head)).contains(x.length - 1)))
      }
    }


    describe("id"){
      it("should return an id uniquely identifying the node if the node exists, and throw an exception otherwise"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          assert(labeledForest.path(labeledForest.id(path)) == path)
        }
        nonExistentPaths.foreach(x => assertThrows[Exception](labeledForest.id(x)))
      }
    }


    describe("idsSubtree"){
      //todo
    }


    describe("path"){
      it("should return the path of a node given a uniquely identifying id of a node if the id is valid, and throw an exception otherwise"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          assert(labeledForest.path(labeledForest.id(path)) == path)
        }
        val invalidId = pathTo__parentLabel_childrenLabels.keySet.map(x => labeledForest.id(x)).max + 1
        nonExistentPaths.foreach(x => assertThrows[Exception](labeledForest.path(invalidId)))
      }
    }


    describe("pathToId"){
      //todo
    }


    describe("paths"){
      it("should return the paths of all nodes in the forest"){
        assert(labeledForest.paths == pathTo__parentLabel_childrenLabels.keys)
      }
    }


    describe("pathsSubtree"){
      //todo
    }


    describe("roots"){
      it("should return the root nodes' labels"){
        assert(labeledForest.roots == rootLabels)
      }
    }


    describe("withLabel"){
      it("should relabel a node if both the node exists and a sibling with the label does not exist"){
        pathTo__parentLabel_childrenLabels.foreach{path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          val relabeledLF = labeledForest.withLabel(path, nonExistentLabel)
          relabeledLF.children(path.slice(0, path.length - 1).:+(nonExistentLabel))
          assertThrows[Exception](relabeledLF.children(path))
        }
      }
      it("should transfer a node's children to the sibling and remove the node if both the node and a sibling with the label exist"){
        pathTo__parentLabel_childrenLabels.foreach { path__parentLabel_childrenLabels =>
          val path = path__parentLabel_childrenLabels._1
          pathTo__parentLabel_childrenLabels.keys.find(x => x != path && x.slice(0, x.length - 1) == path.slice(0, path.length - 1)).foreach { siblingPath =>
            val relabeledLF = labeledForest.withLabel(path, siblingPath.last)
            val childrenLabelsOriginalPath = pathTo__parentLabel_childrenLabels(path)._2
            val childrenLabelsOriginialSiblingPath = pathTo__parentLabel_childrenLabels(siblingPath)._2
            assert(relabeledLF.children(siblingPath) == childrenLabelsOriginialSiblingPath ++ childrenLabelsOriginalPath)
            assertThrows[Exception](relabeledLF.children(path))
          }
        }
      }
      it("should throw an exception if the node does not exist"){
        nonExistentPaths.foreach(x => assertThrows[Exception](labeledForest.withLabel(x, nonExistentLabel)))
      }
    }


    describe("withPath"){
      it("should return a copy of the forest with the path and its subpaths"){
        paths.foreach { path =>
          val newLfPaths = emptyLF.withPath(path).paths
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == subpaths.toSet)
        }
        nonExistentPaths.foreach { path =>
          val newLfPaths = labeledForest.withPath(path).paths
          val subpaths = path.zipWithIndex.map(x => path.take(x._2 + 1))
          assert(newLfPaths == labeledForest.paths.++(subpaths.toSet))
        }
        paths.foreach(x => assert(labeledForest.withPath(x).paths == labeledForest.paths))
      }
    }


    describe("withPaths"){
      it("should return a copy of the forest with the paths and their subpaths"){
        assert(emptyLF.withPaths(pathsToLeaves).paths == labeledForest.paths)
        assert(emptyLF.withPaths(paths).paths == labeledForest.paths)
        assert(labeledForest.withPaths(pathsToLeaves).paths == labeledForest.paths)
        assert(labeledForest.withPaths(paths).paths == labeledForest.paths)
      }
    }


    describe("withSubtreeMoved"){
      it("should move an existing subtree to the child of an existing path outside of the subtree to move if a child does not already exist with the same name"){
        paths.flatMap(x => paths.filter(!_.startsWith(x.init)).map((x, _))).foreach{ pathToMove_newParent =>
          val (pathToMove, newParent) = (pathToMove_newParent._1, pathToMove_newParent._2)
          val withSubtreeMoved = labeledForest.withSubtreeMoved(pathToMove, Some(newParent))
          assert(!withSubtreeMoved.paths.contains(pathToMove))
          paths.filter(_.startsWith(pathToMove)).map(newParent ++ _.drop(pathToMove.init.length)).foreach(x => assert(withSubtreeMoved.paths.contains(x)))
        }
      }
      it("should move an existing subtree to a root if a root does not already exist with the same name"){
        paths.filter(x => !labeledForest.roots.contains(x.last)).foreach{ path =>
          val withSubtreeMoved = labeledForest.withSubtreeMoved(path, None)
          assert(withSubtreeMoved.paths.contains(Seq(path.last)))
          assert(withSubtreeMoved.roots.contains(path.last))
          assert(!withSubtreeMoved.paths.contains(path))
        }
      }
      it("should return an equivalent LabeledForest if pathNewParent is the parent of path"){
        paths.map(x => (x, x.init)).foreach{ pathToMove_newParent =>
          val (pathToMove, newParent) = (pathToMove_newParent._1, pathToMove_newParent._2)
          val withSubtreeMoved = labeledForest.withSubtreeMoved(pathToMove, Some(newParent).filter(_.nonEmpty))
          assert(withSubtreeMoved == labeledForest)
        }
      }
    }


    describe("withoutSubtree"){
      it("should return a copy of the forest without the subtree"){
        paths.foreach{path =>
          val pathsSubtree = paths.filter(_.startsWith(path))
          assert(labeledForest.withoutSubtree(path).paths == paths.toSet.--(pathsSubtree))
        }
      }
    }


    describe("companion object"){

      describe("apply with pathToId"){
        it("should return a LabeledForest with the specified paths and corresponding ids"){
          assert(LabeledForest(labeledForest.pathToId) == labeledForest)
        }
      }

    }

  }
}

object LabeledForestSpec {

  trait LabeledForestTestData {
    type N = Int
    val pathsToLeaves = Set(Seq(0), Seq(1, 2), Seq(3, 4, 5), Seq(3, 4, 6), Seq(3, 7))
    val pathTo__parentLabel_childrenLabels = Map(
      Seq(0) -> (None, Set()),
      Seq(1) -> (None, Set(2)),
      Seq(1, 2) -> (Some(1), Set()),
      Seq(3) -> (None, Set(4, 7)),
      Seq(3, 4) -> (Some(3), Set(5, 6)),
      Seq(3, 4, 5) -> (Some(4), Set()),
      Seq(3, 4, 6) -> (Some(4), Set()),
      Seq(3, 7) -> (Some(3), Set()),
    )
    val paths: Iterable[Seq[Int]] = pathTo__parentLabel_childrenLabels.keys
    val rootLabels = Set(0, 1, 3)
    val validNonExistentPaths = Seq(Seq(8), Seq(0, 0), Seq(0, 1), Seq(2), Seq(1, 1), Seq(2, 1), Seq(2, 2), Seq(1, 2, 1), Seq(1, 2, 2), Seq(1, 2, 0))
    val nonExistentPaths: Seq[Seq[Int]] = validNonExistentPaths.++(Seq())
    val nonExistentLabel: Int = -1
    val emptyLF: LabeledForest[N] = LabeledForest[N]()
    val labeledForest = LabeledForest(pathsToLeaves)
  }

}
