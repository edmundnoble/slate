package dash.ajax

import qq.jsc.Json
import shapeless.{DepFn1, ::, HList, HNil}

import scala.scalajs.js
import scala.scalajs.js.UndefOr

sealed trait PathSegment {
  type T
}
case class StringPathSegment[T0 <: PathSegment](stringPart: String, next: T0) extends PathSegment {
  override type T = T0
}
case class GenericPathSegment[A, T0 <: PathSegment](tag: GenericPathSegmentTag[A], next: T0) extends PathSegment {
  override type T = T0
}
case object PathEnding extends PathSegment {
  override type T = Nothing
}

sealed trait GenericPathSegmentTag[T]
case object BooleanTag extends GenericPathSegmentTag[Boolean]
case class UndefOrTag[A](child: GenericPathSegmentTag[A]) extends GenericPathSegmentTag[UndefOr[A]]
case object IntTag extends GenericPathSegmentTag[Int]
case object DoubleTag extends GenericPathSegmentTag[Double]
case object StringTag extends GenericPathSegmentTag[String]

sealed trait PathToString {
  type P <: PathSegment
  type In <: HList

  def create(path: P, arg: In): String
}

object PathToString {
  type Aux[P0 <: PathSegment, In0 <: HList] = PathToString {type P = P0; type In = In0}
  type Aux1[P0 <: PathSegment] = PathToString {type P = P0}

  implicit def stringPathSegmentArgs[P0 <: PathSegment, In0 <: HList](implicit rest: Aux[P0, In0]): Aux[StringPathSegment[P0], In0] = new PathToString {
    override type P = StringPathSegment[P0]
    override type In = In0
    override def create(path: StringPathSegment[P0], arg: In0) =
      path.stringPart + "/" + rest.create(path.next, arg)
  }
  implicit def genericPathSegmentArgs[A, P0 <: PathSegment, In0 <: HList](implicit rest: Aux[P0, In0]): Aux[GenericPathSegment[A, P0], A :: In0] = new PathToString {
    override type P = GenericPathSegment[A, P0]
    override type In = A :: In0
    override def create(path: GenericPathSegment[A, P0], arg: A :: In0) =
      js.URIUtils.encodeURIComponent(arg.head.toString) + "/" + rest.create(path.next, arg.tail)
  }
  implicit def pathEndingArgsSing[P0]: Aux[PathEnding.type, HNil] = new PathToString {
    override type P = PathEnding.type
    override type In = HNil
    override def create(path: PathEnding.type, arg: HNil) = ""
  }
  @inline
  final def apply[P <: PathSegment](implicit ev: Aux1[P]): ev.type = ev
}

object PathSegment {
  object DSL {
    implicit class pathSegmentOps[P <: PathSegment](seg: P) {
      def :/:(str: String) = StringPathSegment(str, seg)
      def :/:[A](tag: GenericPathSegmentTag[A]) = GenericPathSegment(tag, seg)
    }
  }
}
