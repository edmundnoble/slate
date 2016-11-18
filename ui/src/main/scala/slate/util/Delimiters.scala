package slate
package util

import cats.Applicative
import cats.implicits._
import qq.util.Unsafe

import scala.scalajs.js

sealed abstract class DelimitTransform[O] {
  def to(string: String): Option[O]
  def from(o: O): String
}

final case class DelimitArr[O](transform: DelimitTransform[O], delim: String) extends DelimitTransform[js.Array[O]] {

  import scala.scalajs.js.JSConverters._

  override def to(string: String): Option[js.Array[O]] = {
    import qq.Platform.Js.Unsafe._

    if (string.isEmpty) {
      Some(js.Array())
    } else {
      Unsafe.builderTraverse[js.WrappedArray]
        .traverse[Option, String, O](
        new js.WrappedArray(string.split(delim).toJSArray)
      )(transform.to).map(_.array)
    }
  }

  override def from(o: js.Array[O]): String =
    o.map(transform.from).mkString(delim)

}

final case class DelimitFromBegin[O1, O2](transform1: DelimitTransform[O1], delim: String, transform2: DelimitTransform[O2]) extends DelimitTransform[(O1, O2)] {
  override def to(string: String): Option[(O1, O2)] = {
    val ind = string.indexOf(delim)
    if (ind == -1)
      None
    else {
      val str1 = string.substring(0, ind)
      val str2 = string.substring(ind + delim.length, string.length)
      Applicative[Option].tuple2(
        transform1.to(str1),
        transform2.to(str2)
      )
    }
  }

  override def from(o: (O1, O2)): String =
    transform1.from(o._1) + delim + transform2.from(o._2)
}

final case class DelimitTransformIxmap[O1, O2](transform: DelimitTransform[O1], to: O1 => Option[O2], from: O2 => O1) extends DelimitTransform[O2] {
  override def to(string: String): Option[O2] = transform.to(string).flatMap(to)
  override def from(o: O2): String = transform.from(from(o))
}

case object Unit extends DelimitTransform[String] {
  override def from(o: String): String = o
  override def to(string: String): Option[String] = Some(string)
}

object DelimitTransform {
  final def string: DelimitTransform[String] = Unit
  implicit final class DelimitTransformOps[O](val transform: DelimitTransform[O]) extends AnyVal {
    def splitBy(delim: String): DelimitTransform[js.Array[O]] =
      DelimitArr(transform, delim)
    def |(delim: String): ThenBuilder[O] =
      new ThenBuilder(transform, delim)
    def imap[O2](to: O => O2)(from: O2 => O): DelimitTransform[O2] =
      imapX[O2](o => Some(to(o)))(from)
    def imapX[O2](to: O => Option[O2])(from: O2 => O): DelimitTransform[O2] =
      DelimitTransformIxmap[O, O2](transform, to, from)
  }
  final class ThenBuilder[O](val transform: DelimitTransform[O], val delim: String) {
    def |[O2](otherTransform: DelimitTransform[O2]): DelimitTransform[(O, O2)] =
      DelimitFromBegin(transform, delim, otherTransform)
  }

}
