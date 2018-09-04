package persistence

import connectedForests.ConnectedForests
import play.api.libs.json.{JsValue, Json}

case class ConnectedForestsAndRelatedNodesToFinishedProportionJsonFormat[F, N](
                                                                                forestLabelJsonFormat: JsonFormat[F],
                                                                                nodeValueJsonFormat: JsonFormat[N]
                                                                              ) extends JsonFormat[(ConnectedForests[F, N], Map[(F, Long, F), Double])] {

  override def toJson(connectedForestsAndRelatedNodesToFinishedProportionJsonFormat: (ConnectedForests[F, N], Map[(F, Long, F), Double])): JsValue = {
    Json.toJson((labelToForestAndRelatedNodesJsonFormat.toJson(connectedForestsAndRelatedNodesToFinishedProportionJsonFormat._1.labelToForestAndRelatedNodes),
      connectedForestsAndRelatedNodesToFinishedProportionJsonFormat._2.map(x => (forestLabelJsonFormat.toJson(x._1._1), x._1._2, forestLabelJsonFormat.toJson(x._1._3), x._2))))
  }


  override def fromJson(jsValue: JsValue): (ConnectedForests[F, N], Map[(F, Long, F), Double]) = {
    Some(jsValue.as[(JsValue, Seq[((JsValue, Long, JsValue), Double)])]).map(x => (ConnectedForests(labelToForestAndRelatedNodesJsonFormat.fromJson(x._1)), x._2.map(y =>
      ((forestLabelJsonFormat.fromJson(y._1._1), y._1._2, forestLabelJsonFormat.fromJson(y._1._3)), y._2)).toMap)).get
  }


  private val labelToForestAndRelatedNodesJsonFormat = LabelToForestAndRelatedNodesJsonFormat(forestLabelJsonFormat, nodeValueJsonFormat)

}
