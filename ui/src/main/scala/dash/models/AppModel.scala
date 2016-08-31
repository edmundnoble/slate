package dash.models

import japgolly.scalajs.react.extra.Reusability

case class AppModel(title: Option[String], content: List[ExpandableContentModel])

object AppModel {

  implicit val reusability: Reusability[AppModel] =
    Reusability.caseClass[AppModel]

}
