package persistence

import connectedForests.LabeledForest
import play.api.libs.json.{JsValue, Json}

case class LabelToForestAndRelatedNodesJsonFormat[F, N](
                                                         forestLabelJsonFormat: JsonFormat[F],
                                                         nodeValueJsonFormat: JsonFormat[N]
                                                       ) extends JsonFormat[Map[F, (LabeledForest[N], Map[(Long, F), Set[Long]])]] {

  override def toJson(labelToForestAndRelatedNodesJsonFormat: Map[F, (LabeledForest[N], Map[(Long, F), Set[Long]])]): JsValue = {
    Json.toJson(labelToForestAndRelatedNodesJsonFormat.map(x => (forestLabelJsonFormat.toJson(x._1), (pathToIdJsonFormat.toJson(x._2._1.pathToId),
      x._2._2.map(u => ((u._1._1, forestLabelJsonFormat.toJson(u._1._2)), u._2))))))
  }


  override def fromJson(jsValue: JsValue): Map[F, (LabeledForest[N], Map[(Long, F), Set[Long]])] = {
    jsValue.as[Seq[(JsValue, JsValue)]].map(x => (x._1, x._2.as[((JsValue, JsValue))])).map(x => (x._1, (x._2._1, x._2._2.as[Seq[((Long, JsValue), Set[Long])]]))).map(x => (
      forestLabelJsonFormat.fromJson(x._1), (LabeledForest(pathToIdJsonFormat.fromJson(x._2._1)), x._2._2.map(y => ((y._1._1, forestLabelJsonFormat.fromJson(y._1._2)), y._2))
      .toMap))).toMap
  }


  private val pathToIdJsonFormat = PathToIdJsonFormat(nodeValueJsonFormat)

}
