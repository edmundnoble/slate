package dash
package models

import japgolly.scalajs.react.extra.Reusability

case class AppModel(content: List[ExpandableContentModel]) extends AnyVal

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.caseClass[AppModel]

}
