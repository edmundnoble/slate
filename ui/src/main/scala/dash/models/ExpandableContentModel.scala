package dash.models

import japgolly.scalajs.react.extra.Reusability

case class ExpandableContentModel(title: String, titleUrl: Option[String], content: List[TitledContentModel])

object ExpandableContentModel {

  implicit val reusability: Reusability[ExpandableContentModel] =
    Reusability.caseClass[ExpandableContentModel]

  implicit val pkl =
    upickle.default.macroRW[ExpandableContentModel]

}
