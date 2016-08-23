package dash.ajax

import qq.jsc.Json
import shapeless.{DepFn1, ::, HList, HNil}

import scala.scalajs.js
import scala.scalajs.js.UndefOr

sealed trait PathSegment {
  type T
}
abstract class StringPathSegment(val stringPart: String) extends PathSegment {
  val next: T
}
object StringPathSegment {
  type Aux[T0] = StringPathSegment {type T = T0}
}
abstract class GenericPathSegment[A](val tag: GenericPathSegmentTag[A]) extends PathSegment {
  val next: T
}
object GenericPathSegment {
  type Aux[A, T0] = GenericPathSegment[A] {type T = T0}
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

  implicit def stringPathSegmentArgs[P0 <: PathSegment, In0 <: HList]
  (implicit rest: PathToString.Aux[P0, In0]
  ): Aux[StringPathSegment.Aux[P0], In0] = new PathToString {
    override type P = StringPathSegment.Aux[P0]
    override type In = In0
    override def create(path: StringPathSegment.Aux[P0], arg: In0) =
      path.stringPart + "/" + rest.create(path.next, arg)
  }

  implicit def genericPathSegmentArgs[A, P0 <: PathSegment, In0 <: HList]
  (implicit rest: PathToString.Aux[P0, In0]
  ): PathToString.Aux[GenericPathSegment.Aux[A, P0], A :: In0] = new PathToString {
    override type P = GenericPathSegment.Aux[A, P0]
    override type In = A :: In0
    override def create(path: GenericPathSegment.Aux[A, P0], arg: A :: In0) =
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
    type :/:[A <: PathSegment, B <: PathSegment] = A {type T = B}
    @inline implicit class pathSegmentOps[P <: PathSegment](val seg: P) extends AnyVal {
      @inline final def :/:(str: String): StringPathSegment :/: P = new StringPathSegment(str) {
        override val next = seg
        override type T = P
      }
      @inline final def :/:[A](tag: GenericPathSegmentTag[A]): GenericPathSegment[A] :/: P = new GenericPathSegment(tag) {
        override val next = seg
        override type T = P
      }
    }
  }
}
