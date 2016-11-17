package slate
package models

import cats.implicits._
import upickle.Js

import scala.scalajs.js
import scala.util.Try

case class DatedAppContent(content: List[ExpandableContentModel], date: js.Date)

object DatedAppContent {

  implicit val pkl: upickle.default.Reader[DatedAppContent] with upickle.default.Writer[DatedAppContent] =
    new upickle.default.Reader[DatedAppContent] with upickle.default.Writer[DatedAppContent] {
      override def read0: PartialFunction[Js.Value, DatedAppContent] = Function.unlift[Js.Value, DatedAppContent] {
        case o: Js.Obj =>
          for {
            content <- Try(o("content").arr).toOption.flatMap(_.toList.traverse(ExpandableContentModel.pkl.read.lift))
            date <- Try(o("date").num).toOption.map(new js.Date(_))
          } yield DatedAppContent(content, date)
        case _ => None
      }

      override def write0: DatedAppContent => Js.Value = { m =>
        Js.Obj("date" -> Js.Num(m.date.getTime()), "content" -> Js.Arr(m.content.map(ExpandableContentModel.pkl.write): _*))
      }
    }

}
