package dash.models

import japgolly.scalajs.react.extra.Reusability

case class TitledContentModel(title: String, titleUrl: Option[String], content: String)

object TitledContentModel {

  implicit val reusability: Reusability[TitledContentModel] =
    Reusability.caseClass[TitledContentModel]

}

