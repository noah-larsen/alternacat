package persistence

import connectedForests.LabeledForest
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}

case class PathToIdJsonFormat[N](nodeValueJsonFormat: JsonFormat[N]) extends JsonFormat[Map[Seq[N], Long]] {

  override def toJson(pathToId: Map[Seq[N], Long]): JsValue = {
    Json.toJson(pathToId.map(x => (x._1.map(nodeValueJsonFormat.toJson), x._2)))
  }


  override def fromJson(jsValue: JsValue): Map[Seq[N], Long] = {
    jsValue.as[Seq[(Seq[JsValue], Long)]].map(x => (x._1.map(nodeValueJsonFormat.fromJson), x._2)).toMap
  }

}