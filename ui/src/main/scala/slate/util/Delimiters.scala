package slate
package util

import cats.Applicative
import cats.implicits._
import qq.util.Unsafe

import scalajs.js

sealed abstract class DelimitTransform[O] {
  def toInterpret(string: String): Option[O]
  def fromInterpret(o: O): String
}
final case class DelimitArr[O](transform: DelimitTransform[O], delim: String) extends DelimitTransform[js.Array[O]] {

  import scala.scalajs.js.JSConverters._

  override def toInterpret(string: String): Option[js.Array[O]] = {
    import qq.Platform.Js.Unsafe._

    Unsafe.builderTraverse[js.WrappedArray]
      .traverse[Option, String, O](
      new js.WrappedArray(string.split(delim).toJSArray)
    )(transform.toInterpret).map(_.array)
  }

  override def fromInterpret(o: js.Array[O]): String =
    o.map(transform.fromInterpret).mkString(delim)

}

final case class DelimitFromBegin[O1, O2](transform1: DelimitTransform[O1], delim: String, transform2: DelimitTransform[O2]) extends DelimitTransform[(O1, O2)] {

  override def toInterpret(string: String): Option[(O1, O2)] = {
    val ind = string.indexOf(delim)
    if (ind == -1)
      None
    else
      Applicative[Option].tuple2(
        transform1.toInterpret(string.substring(0, ind)),
        transform2.toInterpret(string.substring(ind + delim.length, string.length))
      )
  }

  override def fromInterpret(o: (O1, O2)): String =
    transform1.fromInterpret(o._1) + delim + transform2.fromInterpret(o._2)
}

final case class DelimitTransformIxmap[O1, O2](transform: DelimitTransform[O1], to: O1 => Option[O2], from: O2 => O1) extends DelimitTransform[O2] {
  override def toInterpret(string: String): Option[O2] = transform.toInterpret(string).flatMap(to)
  override def fromInterpret(o: O2): String = transform.fromInterpret(from(o))
}

case object Unit extends DelimitTransform[String] {
  override def fromInterpret(o: String): String = o
  override def toInterpret(string: String): Option[String] = Some(string)
}

object DelimitTransform {
  final def id: DelimitTransform[String] = Unit
  implicit final class DelimitTransformOps[O](val transform: DelimitTransform[O]) extends AnyVal {
    def thenDelimitBy(delim: String): DelimitTransform[js.Array[O]] =
      DelimitArr(transform, delim)
    def joinWithDelimiter[O2](delim: String, other: DelimitTransform[O2]): DelimitTransform[(O, O2)] =
      DelimitFromBegin(transform, delim, other)
    def imap[O2](to: O => O2, from: O2 => O): DelimitTransform[O2] =
      imapX[O2](o => Some(to(o)), from)
    def imapX[O2](to: O => Option[O2], from: O2 => O): DelimitTransform[O2] =
      DelimitTransformIxmap[O, O2](transform, to, from)
  }

}
