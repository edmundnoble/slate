package slate
package util

import cats.Applicative
import cats.implicits._
import qq.util.Unsafe

import scala.scalajs.js

// An interface for codecs that deserialize from String to O, with the possibility of failure,
// and serialize from O to String in a way guaranteed to succeed.
// A lawful (trans: DelimitTransform[O]) will satisfy the laws:
// forall (str: String). trans.to(str).map(from).forall(_ == str)
// forall (o: O). trans.to(trans.from(o)) == Some(o)
// i.e., a DelimitTransform[O] is an injection from O to String, with an explicit inverse.
sealed abstract class DelimitTransform[O] {
  def to(string: String): Option[O]
  def from(o: O): String
}

// forall (transform: DelimitTransform[O]), (delim: String).
// DelimitSplit(transform, delim): DelimitTransform[Array[O]] represents the reversible function that:
// when converting to a string, is joining an array of strings by a delimiter,
// and when converting from a string, is splitting a string by a delimiter
final case class DelimitSplit[O](transform: DelimitTransform[O], delim: String) extends DelimitTransform[js.Array[O]] {

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

// forall (transform1: DelimitTransform[O1]), (transform2: DelimitTransform[O2]), (delim: String).
// DelimitFromBegin(transform1, delim, transform2): DelimitTransform[Array[O]] represents the reversible function that:
// when converting to a string, performs transform1.from and transform2.from on the elements of a tuple and joins their output with delim,
// and when converting from a String str,
// 1 finds the first index of delim in str
// 2 i applies transform1.to to the substring of str up to that point (exclusive) and
//   ii applies transform2.to to the substring of str after the delimiter and
//   iii tuples the two together
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

// This is a free invariant functor, but `to` is partial.
// This lets you extend DelimitTransforms with your own injections.
final case class DelimitTransformIxmap[O1, O2](transform: DelimitTransform[O1], to: O1 => Option[O2], from: O2 => O1) extends DelimitTransform[O2] {
  override def to(string: String): Option[O2] = transform.to(string).flatMap(to)
  override def from(o: O2): String = transform.from(from(o))
}

// The identity. Strings can always be deserialized to Strings.
case object Unit extends DelimitTransform[String] {
  override def from(o: String): String = o
  override def to(string: String): Option[String] = Some(string)
}

// The DSL you can use for practically using the above classes.
object DelimitTransform {
  final def string: DelimitTransform[String] = Unit
  implicit final class DelimitTransformOps[O](val transform: DelimitTransform[O]) extends AnyVal {
    def splitBy(delim: String): DelimitTransform[js.Array[O]] =
      DelimitSplit(transform, delim)
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
