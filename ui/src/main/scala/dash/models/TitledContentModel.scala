package dash.models

case class TitledContentModel(title: String, titleUrl: Option[String], content: String)

object TitledContentModel {
  implicit val pkl = upickle.default.macroRW[TitledContentModel]
}

