package qq
package data

import matryoshka.{Corecursive, Fix, Mu, Nu}

import scala.language.higherKinds
import scalaz.\/

// smart constructors that make a FilterComponent node including its children
object FilterDSL {

  class Dsl[T[_[_]] : Corecursive] {
    @inline final def id: T[FilterComponent] =
      FilterComponent.embed[T](IdFilter())

    @inline final def compose(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](ComposeFilters(first, second))

    @inline final def silence(f: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](SilenceExceptions(f))

    @inline final def enlist(f: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](EnlistFilter(f))

    @inline final def collectResults(f: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](CollectResults(f))

    @inline final def ensequence(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](EnsequenceFilters(first, second))

    @inline final def enject(obj: List[((String \/ T[FilterComponent]), T[FilterComponent])]): T[FilterComponent] =
      FilterComponent.embed[T](EnjectFilters(obj))

    @inline final def call(name: String, params: List[T[FilterComponent]] = Nil): T[FilterComponent] =
      FilterComponent.embed[T](CallFilter(name, params))

    @inline final def selectKey(key: String): T[FilterComponent] =
      FilterComponent.embed[T](SelectKey(key))

    @inline final def selectIndex(index: Int): T[FilterComponent] =
      FilterComponent.embed[T](SelectIndex(index))

    @inline final def selectRange(start: Int, end: Int): T[FilterComponent] =
      FilterComponent.embed[T](SelectRange(start, end))

    @inline final def add(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](AddFilters(first, second))

    @inline final def subtract(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](SubtractFilters(first, second))

    @inline final def multiply(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](MultiplyFilters(first, second))

    @inline final def divide(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](DivideFilters(first, second))

    @inline final def modulo(first: T[FilterComponent], second: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](ModuloFilters(first, second))

    @inline final def constNumber(value: Double): T[FilterComponent] =
      FilterComponent.embed[T](ConstNumber(value))

    @inline final def constString(value: String): T[FilterComponent] =
      FilterComponent.embed[T](ConstString(value))

    @inline final def letAsBinding(name: String, as: T[FilterComponent], in: T[FilterComponent]): T[FilterComponent] =
      FilterComponent.embed[T](LetAsBinding[T[FilterComponent]](name, as, in))

    @inline final def deref(name: String): T[FilterComponent] =
      FilterComponent.embed[T](Dereference[T[FilterComponent]](name))
  }

  final def dsl[T[_[_]] : Corecursive]: Dsl[T] = new Dsl[T]
  final val mu: Dsl[Mu] = dsl[Mu]
  final val nu: Dsl[Nu] = dsl[Nu]
  final val fix: Dsl[Fix] = dsl[Fix]

}
