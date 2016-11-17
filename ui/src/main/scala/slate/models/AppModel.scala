package slate
package models

import slate.app.SlateApp.AllErrors
import japgolly.scalajs.react.extra.Reusability

import scala.scalajs.js

case class AppModel(content: List[ExpandableContentModel], updated: js.Date)

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
