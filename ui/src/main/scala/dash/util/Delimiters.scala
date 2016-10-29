package dash.util

import cats.Applicative
import cats.implicits._
import qq.util.Unsafe

import scalajs.js

sealed abstract class DelimitTransformSym[O] {
  def toInterpret(sym: DelimitTransform[O], string: String): Option[O]
  def fromInterpret(sym: DelimitTransform[O], o: O): String
}

object DelimitTransformSym {

  implicit val transformUnit: DelimitTransformSym[String] = new DelimitTransformSym[String] {
    override def fromInterpret(sym: DelimitTransform[String], o: String): String = o
    override def toInterpret(sym: DelimitTransform[String], string: String): Option[String] = Some(string)
  }

  implicit def transformDelim[O](implicit transform: DelimitTransformSym[O]): DelimitTransformSym[js.Array[O]] =
    new DelimitTransformSym[js.Array[O]] {

      import qq.Platform.Js.Unsafe._

      override def toInterpret(sym: DelimitTransform[js.Array[O]], string: String): Option[js.Array[O]] = sym match {
        case DelimitArr(nextSym, delim) =>
          Unsafe.builderTraverse[js.WrappedArray]
            .traverse[Option, String, O](
            new js.WrappedArray(string.split(delim).asInstanceOf[js.Array[String]])
          )(transform.toInterpret(nextSym, _)).map(_.array)
      }
      override def fromInterpret(sym: DelimitTransform[js.Array[O]], o: js.Array[O]): String = sym match {
        case DelimitArr(nextSym, delim) => o.map(transform.fromInterpret(nextSym, _)).mkString(delim)
      }
    }

  implicit def transformDelimFromBegin[O1, O2](implicit transform1: DelimitTransformSym[O1], transform2: DelimitTransformSym[O2]): DelimitTransformSym[(O1, O2)] =
    new DelimitTransformSym[(O1, O2)] {

      override def toInterpret(sym: DelimitTransform[(O1, O2)], string: String): Option[(O1, O2)] = sym match {
        case DelimitFromBegin(nextSym, delim, nextSym2) =>
          val ind = string.indexOf(string)
          if (ind == -1) {
            None
          } else {
            Applicative[Option].tuple2(
              transform1.toInterpret(nextSym, string.substring(0, ind)),
              transform2.toInterpret(nextSym2, string.substring(0, ind))
            )
          }
      }
      override def fromInterpret(sym: DelimitTransform[(O1, O2)], o: (O1, O2)): String = sym match {
        case DelimitFromBegin(nextSym, delim, nextSym2) =>
          transform1.fromInterpret(nextSym, o._1) + delim + transform2.fromInterpret(nextSym2, o._2)
      }
    }

}

sealed abstract class DelimitTransform[O]
final case class DelimitArr[O](transform: DelimitTransform[O], delim: String) extends DelimitTransform[js.Array[O]]
final case class DelimitFromBegin[O1, O2](transform1: DelimitTransform[O1], delim: String, transform2: DelimitTransform[O2]) extends DelimitTransform[(O1, O2)]
case object Unit extends DelimitTransform[String]

object DelimitTransform {
  final def id: DelimitTransform[String] = Unit
  implicit final class DelimitTransformOps[O](val transform: DelimitTransform[O]) {
    def thenDelimitBy(delim: String): DelimitTransform[js.Array[O]] = DelimitArr(transform, delim)
    def joinWithDelimiter[O2](delim: String, other: DelimitTransform[O2]): DelimitTransform[(O, O2)] = DelimitFromBegin(transform, delim, other)
  }


  final def interpret[O](delimitTransform: DelimitTransform[O])(implicit delimitTransformSym: DelimitTransformSym[O]): (String => Option[O], O => String) = {
    (delimitTransformSym.toInterpret(delimitTransform, _: String), delimitTransformSym.fromInterpret(delimitTransform, _: O))
  }
}
