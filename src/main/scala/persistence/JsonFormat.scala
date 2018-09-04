package persistence

import play.api.libs.json.JsValue

trait JsonFormat[T] {

  def toJson(t: T): JsValue
  def fromJson(jsValue: JsValue): T

}
