package dash
package models

import japgolly.scalajs.react.extra.Reusability
import upickle.Js

import scala.util.Try
import scalaz.syntax.std.option._

case class ExpandableContentModel(title: String, titleUrl: Option[String], content: List[TitledContentModel])

object ExpandableContentModel {

  implicit val reusability: Reusability[ExpandableContentModel] =
    Reusability.caseClass[ExpandableContentModel]

  implicit val pkl: upickle.default.Reader[ExpandableContentModel] with upickle.default.Writer[ExpandableContentModel] =
    new upickle.default.Reader[ExpandableContentModel] with upickle.default.Writer[ExpandableContentModel] {
      override def read0: PartialFunction[Js.Value, ExpandableContentModel] = {
        case o: Js.Obj => ExpandableContentModel(
          o("title").str,
          o.value.toMap.get("titleUrl").map(_.str),
          o("content").arr.flatMap(TitledContentModel.pkl.read.lift(_))(collection.breakOut))
      }

      override def write0: ExpandableContentModel => Js.Value = { m =>
        val fields = Seq[Option[(String, Js.Value)]](("title" -> Js.Str(m.title)).some, m.titleUrl.map[(String, Js.Value)](f => "titleUrl" -> Js.Str(f)),
          ("content" -> Js.Arr(m.content.map(TitledContentModel.pkl.write): _*)).some).flatten
        Js.Obj(fields: _*)
      }
    }

}
