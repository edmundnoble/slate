package dash
package models

import japgolly.scalajs.react.extra.Reusability

import scalaz.\/

case class AppModel(content: Throwable \/ List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.byRefOr_==[AppModel]

}
