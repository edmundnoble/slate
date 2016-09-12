package dash
package models

import japgolly.scalajs.react.extra.Reusability
import upickle.Js

import scala.util.Try

case class TitledContentModel(title: String, titleUrl: Option[String], content: String)

object TitledContentModel {

  implicit val reusability: Reusability[TitledContentModel] =
    Reusability.caseClass[TitledContentModel]

  implicit val pkl: upickle.default.Reader[TitledContentModel] with upickle.default.Writer[TitledContentModel] =
    new upickle.default.Reader[TitledContentModel] with upickle.default.Writer[TitledContentModel] {
    override def read0: PartialFunction[Js.Value, TitledContentModel] = {
      case o: Js.Obj => TitledContentModel(
        o("title").str,
        Try(o("titleUrl").str).toOption,
        o("content").str)
    }
    override def write0: TitledContentModel => Js.Value = { m =>
      val fields = Seq[Option[(String, Js.Value)]](Some("title" -> Js.Str(m.title)), m.titleUrl.map(f => "titleUrl" -> Js.Str(f)),
        Some("content" -> Js.Str(m.content))).flatten
      Js.Obj(fields: _*)
    }
  }


}

