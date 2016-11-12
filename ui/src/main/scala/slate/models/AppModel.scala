package slate
package models

import slate.app.SlateApp.AllErrors
import japgolly.scalajs.react.extra.Reusability
import cats.data.Xor

case class AppModel(content: AllErrors Xor List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
