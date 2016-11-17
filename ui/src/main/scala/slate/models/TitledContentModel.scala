package slate
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
      override def read0: PartialFunction[Js.Value, TitledContentModel] = Function.unlift {
        case o: Js.Obj =>
          for {
            title <- Try(o("title").str).toOption
            titleUrl = Try(o("titleUrl").str).toOption
            content <- Try(o("content").str).toOption
          } yield TitledContentModel(title, titleUrl, content)
        case _ => None
      }
      override def write0: TitledContentModel => Js.Value = { m =>
        val fields = Seq[Option[(String, Js.Value)]](Some("title" -> Js.Str(m.title)), m.titleUrl.map(f => "titleUrl" -> Js.Str(f)),
          Some("content" -> Js.Str(m.content))).flatten
        Js.Obj(fields: _*)
      }
    }


}

