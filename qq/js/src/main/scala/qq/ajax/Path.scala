package qq.ajax

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil}

import scala.scalajs.js
import scala.scalajs.js.UndefOr

sealed trait PathSegment {
  type T
}
abstract class ConstantPathSegment(val stringPart: String) extends PathSegment {
  val next: T
}
object ConstantPathSegment {
  type Aux[T0] = ConstantPathSegment {type T = T0}
}
abstract class GenericPathSegment[K, A](val tag: FieldType[K, GenericPathSegmentTag[A]]) extends PathSegment {
  val next: T
}
object GenericPathSegment {
  type Aux[K, A, T0] = GenericPathSegment[K, A] {type T = T0}
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
  ): Aux[ConstantPathSegment.Aux[P0], In0] = new PathToString {
    override type P = ConstantPathSegment.Aux[P0]
    override type In = In0
    override def create(path: ConstantPathSegment.Aux[P0], arg: In0) =
      path.stringPart + "/" + rest.create(path.next, arg)
  }

  implicit def genericPathSegmentArgs[K, A, P0 <: PathSegment, In0 <: HList]
  (implicit rest: PathToString.Aux[P0, In0]
  ): PathToString.Aux[GenericPathSegment.Aux[K, A, P0], FieldType[K, A] :: In0] = new PathToString {
    override type P = GenericPathSegment.Aux[K, A, P0]
    override type In = FieldType[K, A] :: In0
    override def create(path: GenericPathSegment.Aux[K, A, P0], arg: FieldType[K, A] :: In0) =
      js.URIUtils.encodeURIComponent((arg.head: A).toString) + "/" + rest.create(path.next, arg.tail)
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
      @inline final def :/:(str: String): ConstantPathSegment :/: P = new ConstantPathSegment(str) {
        override val next = seg
        override type T = P
      }
      @inline final def :/:[K, A](tag: FieldType[K, GenericPathSegmentTag[A]]): GenericPathSegment[K, A] :/: P = new GenericPathSegment(tag) {
        override val next = seg
        override type T = P
      }
    }
  }
}
