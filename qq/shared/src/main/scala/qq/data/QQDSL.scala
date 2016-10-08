package qq
package data

import matryoshka.{Corecursive, Fix, Mu, Nu}
import qq.data.FilterComponent.embed

import scala.language.{higherKinds, implicitConversions}
import scalaz.\/

// smart constructors that make a FilterComponent node including its children
object QQDSL {

  final class Dsl[T[_[_]] : Corecursive] {
    @inline def collectResults: PathComponent =
      CollectResults

    @inline def selectKey(key: String): PathComponent =
      SelectKey(key)

    @inline def selectIndex(index: Int): PathComponent =
      SelectIndex(index)

    @inline def selectRange(start: Int, end: Int): PathComponent =
      SelectRange(start, end)

    implicit final class extraOps(val f: T[FilterComponent]) {
      final def |(next: T[FilterComponent]): T[FilterComponent] = compose(f, next)

      final def +(next: T[FilterComponent]): T[FilterComponent] = add(f, next)

      final def ===(next: T[FilterComponent]): T[FilterComponent] = equal(f, next)

      final def -(next: T[FilterComponent]): T[FilterComponent] = subtract(f, next)

      final def *(next: T[FilterComponent]): T[FilterComponent] = multiply(f, next)

      final def /(next: T[FilterComponent]): T[FilterComponent] = divide(f, next)

      final def %(next: T[FilterComponent]): T[FilterComponent] = modulo(f, next)
    }

    implicit def embedStr(s: String): T[FilterComponent] = constString(s)

    implicit def embedInt(d: Int): T[FilterComponent] = constNumber(d)

    implicit def embedDouble(d: Double): T[FilterComponent] = constNumber(d)

    @inline def id: T[FilterComponent] =
      getPath(Nil)

    @inline implicit def getPath(components: List[PathComponent]): T[FilterComponent] = {
      embed[T](PathOperation(components, PathGet))
    }

    @inline implicit def getPathS(component: PathComponent): T[FilterComponent] = {
      getPath(component :: Nil)
    }

    @inline def setPath(components: List[PathComponent], value: T[FilterComponent]): T[FilterComponent] = {
      embed[T](PathOperation(components, PathSet(value)))
    }

    @inline def modifyPath(components: List[PathComponent], modify: T[FilterComponent]): T[FilterComponent] = {
      embed[T](PathOperation(components, PathModify(modify)))
    }

    @inline def define(name: String, params: List[String], body: T[FilterComponent]): Definition[T[FilterComponent]] =
      Definition[T[FilterComponent]](name, params, body)

    @inline def compose(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](ComposeFilters(first, second))

    @inline def silence(f: T[FilterComponent]): T[FilterComponent] =
      embed[T](SilenceExceptions(f))

    @inline def enlist(f: T[FilterComponent]): T[FilterComponent] =
      embed[T](EnlistFilter(f))

    @inline def ensequence(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](EnsequenceFilters(first, second))

    @inline def enject(obj: List[((String \/ T[FilterComponent]), T[FilterComponent])]): T[FilterComponent] =
      embed[T](EnjectFilters(obj))

    @inline def call(name: String, params: List[T[FilterComponent]] = Nil): T[FilterComponent] =
      embed[T](CallFilter(name, params))

    @inline def add(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Add))

    @inline def subtract(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Subtract))

    @inline def multiply(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Multiply))

    @inline def divide(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Divide))

    @inline def modulo(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Modulo))

    @inline def equal(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      embed[T](FilterMath(first, second, Equal))

    @inline def not: T[FilterComponent] =
      embed[T](FilterNot())

    @inline def constNumber(value: Double): T[FilterComponent] =
      embed[T](ConstNumber(value))

    @inline def constString(value: String): T[FilterComponent] =
      embed[T](ConstString(value))

    @inline def constBoolean(value: Boolean): T[FilterComponent] =
      embed[T](ConstBoolean(value))

    @inline def asBinding(name: String, as: T[FilterComponent], in: T[FilterComponent]): T[FilterComponent] =
      embed[T](AsBinding[T[FilterComponent]](name, as, in))

    @inline def deref(name: String): T[FilterComponent] =
      embed[T](Dereference[T[FilterComponent]](name))
  }

  final def dsl[T[_[_]] : Corecursive]: Dsl[T] = new Dsl[T]

  val mu: Dsl[Mu] = dsl[Mu]
  val nu: Dsl[Nu] = dsl[Nu]
  val fix: Dsl[Fix] = dsl[Fix]

}
