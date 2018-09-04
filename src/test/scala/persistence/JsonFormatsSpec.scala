package persistence

import connectedForests.ConnectedForestsSpec.ConnectedForestsTestData
import connectedForests.DevelopingConnectedForestsSpec.DevelopingConnectedForestsTestData
import connectedForests.{ConnectedForests, DevelopingConnectedForests, LabeledForest}
import org.scalatest.FunSpec
import persistence.JsonFormatsSpec.IntJsonFormat
import play.api.libs.json.{JsValue, Json}

class JsonFormatsSpec extends FunSpec with DevelopingConnectedForestsTestData {

  describe("PathToIdJsonFormat"){
    it("can convert and reconstruct a labeled forest"){
      val pathToIdJsonFormat = PathToIdJsonFormat(IntJsonFormat)
      assert(LabeledForest(pathToIdJsonFormat.fromJson(pathToIdJsonFormat.toJson(labeledForest.pathToId))) == labeledForest)
    }
  }


  describe("LabelToForestAndRelatedNodesJsonFormat"){
    it("can convert and reconstruct a connected forest"){
      val labelToForestAndRelatedNodesJsonFormat = LabelToForestAndRelatedNodesJsonFormat(IntJsonFormat, IntJsonFormat)
      assert(ConnectedForests(labelToForestAndRelatedNodesJsonFormat.fromJson(labelToForestAndRelatedNodesJsonFormat.toJson(connectedForests.labelToForestAndRelatedNodes)))
        == connectedForests)
    }
  }


  describe(ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat.getClass.getSimpleName){
    it("can convert and reconstruct a developing connected forest"){
      val connectedForestsAndRelatedNodesToFinishedProportionJsonFormat = ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat(IntJsonFormat, IntJsonFormat)
      assert(DevelopingConnectedForests(connectedForestsAndRelatedNodesToFinishedProportionJsonFormat.fromJson(connectedForestsAndRelatedNodesToFinishedProportionJsonFormat
        .toJson(dcf.connectedForestsAndRelatedNodesToFinishedProportion))) == dcf)
      //todo test with finished proportions
    }
  }

}

object JsonFormatsSpec {

  private object IntJsonFormat extends JsonFormat[Int] {

    override def toJson(int: Int): JsValue = {
      Json.toJson(int)
    }


    override def fromJson(jsValue: JsValue): Int = {
      jsValue.as[Int]
    }

  }

}
