package slate
package models

import cats.implicits._
import japgolly.scalajs.react.extra.Reusability
import upickle.Js

import scala.util.Try

case class ExpandableContentModel(title: String, titleUrl: Option[String], content: List[TitledContentModel])

object ExpandableContentModel {

  implicit val reusability: Reusability[ExpandableContentModel] =
    Reusability.caseClass[ExpandableContentModel]

  implicit val pkl: upickle.default.Reader[ExpandableContentModel] with upickle.default.Writer[ExpandableContentModel] =
    new upickle.default.Reader[ExpandableContentModel] with upickle.default.Writer[ExpandableContentModel] {
      override def read0: PartialFunction[Js.Value, ExpandableContentModel] = Function.unlift[Js.Value, ExpandableContentModel] {
        case o: Js.Obj =>
          for {
            title <- Try(o("title").str).toOption
            titleUrl = Try(o("titleUrl").str).toOption
            content <- Try(o("content").arr.toList).toOption.flatMap(_.traverse(TitledContentModel.pkl.read.lift))
          } yield ExpandableContentModel(title, titleUrl, content)
        case _ => None
      }

      override def write0: ExpandableContentModel => Js.Value = { m =>
        val fields = Seq[Option[(String, Js.Value)]](("title" -> Js.Str(m.title)).some, m.titleUrl.map[(String, Js.Value)](f => "titleUrl" -> Js.Str(f)),
          ("content" -> Js.Arr(m.content.map(TitledContentModel.pkl.write): _*)).some).flatten
        Js.Obj(fields: _*)
      }
    }

  implicit val pkls: upickle.default.Reader[List[ExpandableContentModel]] with upickle.default.Writer[List[ExpandableContentModel]] =
    new upickle.default.Reader[List[ExpandableContentModel]] with upickle.default.Writer[List[ExpandableContentModel]] {
      override def read0: PartialFunction[Js.Value, List[ExpandableContentModel]] = Function.unlift[Js.Value, List[ExpandableContentModel]] {
        case o: Js.Obj =>
          for {
            objs <- Try(o.arr.toList).toOption
            readed <- objs.traverse(pkl.read.lift)
          } yield readed
        case _ => None
      }

      override def write0: List[ExpandableContentModel] => Js.Value = { ms =>
        val objects: List[Js.Obj] = ms.map(m => Js.Obj(Seq(
          ("title" -> Js.Str(m.title)).some,
          m.titleUrl.map[(String, Js.Value)](f => "titleUrl" -> Js.Str(f)),
          ("content" -> Js.Arr(m.content.map(TitledContentModel.pkl.write): _*)).some).flatten: _*
        ))
        Js.Arr(objects: _*)
      }
    }
}
