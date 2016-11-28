package slate
package models

import cats.implicits._
import japgolly.scalajs.react.extra.Reusability
import upickle.Js
import upickle.default.{Reader, Writer}

case class ExpandableContentModel(title: String, titleUrl: Option[String], content: List[TitledContentModel])

object ExpandableContentModel {

  implicit val reusability: Reusability[ExpandableContentModel] =
    Reusability.caseClass[ExpandableContentModel]

  implicit val pkl: Reader[ExpandableContentModel] with Writer[ExpandableContentModel] =
    new Reader[ExpandableContentModel] with Writer[ExpandableContentModel] {
      override def read0: PartialFunction[Js.Value, ExpandableContentModel] = Function.unlift[Js.Value, ExpandableContentModel] {
        case Js.Obj(pairs@_*) =>
          for {
            title <- pairs.find(_._1 == "title").flatMap(_._2 match {
              case Js.Str(str) => Some(str)
              case _ => None
            })
            titleUrl = pairs.find(_._1 == "titleUrl").flatMap(_._2 match {
              case Js.Str(str) => Some(str)
              case _ => None
            })
            content <- pairs.find(_._1 == "content").flatMap(_._2 match {
              case Js.Arr(values@_*) => values.toList.traverse(TitledContentModel.pkl.read.lift)
              case _ => None
            })
          } yield ExpandableContentModel(title, titleUrl, content)
        case _ => None
      }

      override def write0: ExpandableContentModel => Js.Value = { m =>
        val fields = Seq[Option[(String, Js.Value)]](("title" -> Js.Str(m.title)).some, m.titleUrl.map[(String, Js.Value)](f => "titleUrl" -> Js.Str(f)),
          ("content" -> Js.Arr(m.content.map(TitledContentModel.pkl.write): _*)).some).flatten
        Js.Obj(fields: _*)
      }
    }

  implicit val pkls: Reader[List[ExpandableContentModel]] with Writer[List[ExpandableContentModel]] =
    new Reader[List[ExpandableContentModel]] with Writer[List[ExpandableContentModel]] {
      override def read0: PartialFunction[Js.Value, List[ExpandableContentModel]] = Function.unlift[Js.Value, List[ExpandableContentModel]] {
        case o: Js.Arr =>
          o.value.toList.traverse(pkl.read.lift)
        case _ => None
      }

      override def write0: List[ExpandableContentModel] => Js.Value = { ms =>
        Js.Arr(ms.map { m =>
          val otherFields = List(
            "title" -> Js.Str(m.title),
            "content" -> Js.Arr(m.content.map(TitledContentModel.pkl.write): _*)
          )
          Js.Obj(m.titleUrl.fold(otherFields)(u => ("titleUrl" -> Js.Str(u)) :: otherFields): _*)
        }: _*)
      }
    }
}
