package slate
package models

import upickle.Js

import scala.scalajs.js

final case class DatedAppContent(content: List[ExpandableContentModel], date: js.Date)

object DatedAppContent {

  implicit val pkl: upickle.default.Reader[DatedAppContent] with upickle.default.Writer[DatedAppContent] =
    new upickle.default.Reader[DatedAppContent] with upickle.default.Writer[DatedAppContent] {
      override def read0: PartialFunction[Js.Value, DatedAppContent] = Function.unlift[Js.Value, DatedAppContent] {
        case Js.Obj(values@_*) =>
          for {
            content <- values.find(_._1 == "content").flatMap(t => ExpandableContentModel.pkls.read.lift(t._2))
            date <- values.find(_._1 == "date").flatMap {
              case (_, Js.Num(n)) => Some(new js.Date(n))
              case _ => None
            }
          } yield DatedAppContent(content, date)
        case _ => None
      }

      override def write0: DatedAppContent => Js.Value = { m =>
        Js.Obj("date" -> Js.Num(m.date.getTime()), "content" -> ExpandableContentModel.pkls.write(m.content))
      }
    }

}
