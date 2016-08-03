package dash.models

case class ExpandableContentModel(title: String, titleUrl: Option[String], content: List[TitledContentModel])

object ExpandableContentModel {
  implicit val pkl = upickle.default.macroRW[ExpandableContentModel]
}
