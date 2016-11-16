package qq
package data

import qq.data.FilterComponent.embed

import scala.language.{higherKinds, implicitConversions}

import qq.util.Fix

// smart constructors that make a FilterComponent node including its children
object QQDSL {

    @inline def collectResults: PathComponent =
      CollectResults

    @inline def selectKey(key: String): PathComponent =
      SelectKey(key)

    @inline def selectIndex(index: Int): PathComponent =
      SelectIndex(index)

    @inline def selectRange(start: Int, end: Int): PathComponent =
      SelectRange(start, end)

    implicit final class extraOps(val f: ConcreteFilter) {
      def |(next: ConcreteFilter): ConcreteFilter = compose(f, next)
      def |+|(next: ConcreteFilter): ConcreteFilter = ensequence(f, next)
      def +(next: ConcreteFilter): ConcreteFilter = add(f, next)
      def ===(next: ConcreteFilter): ConcreteFilter = equal(f, next)
      def <==(next: ConcreteFilter): ConcreteFilter = lte(f, next)
      def >==(next: ConcreteFilter): ConcreteFilter = gte(f, next)
      def <(next: ConcreteFilter): ConcreteFilter = lessThan(f, next)
      def >(next: ConcreteFilter): ConcreteFilter = greaterThan(f, next)
      def -(next: ConcreteFilter): ConcreteFilter = subtract(f, next)
      def *(next: ConcreteFilter): ConcreteFilter = multiply(f, next)
      def /(next: ConcreteFilter): ConcreteFilter = divide(f, next)
      def %(next: ConcreteFilter): ConcreteFilter = modulo(f, next)
    }

    implicit def embedStr(s: String): ConcreteFilter = constString(s)

    implicit def embedInt(d: Int): ConcreteFilter = constNumber(d)

    implicit def embedDouble(d: Double): ConcreteFilter = constNumber(d)

    @inline def id: ConcreteFilter =
      getPath(Nil)

    @inline implicit def getPath(components: List[PathComponent]): ConcreteFilter = {
      Fix(PathOperation(components, PathGet))
    }

    @inline implicit def getPathS(component: PathComponent): ConcreteFilter = {
      getPath(component :: Nil)
    }

    @inline def setPath(components: List[PathComponent], value: ConcreteFilter): ConcreteFilter = {
      Fix(PathOperation(components, PathSet(value)))
    }

    @inline def modifyPath(components: List[PathComponent], modify: ConcreteFilter): ConcreteFilter = {
      Fix(PathOperation(components, PathModify(modify)))
    }

    @inline def define(name: String, params: List[String], body: ConcreteFilter): Definition[ConcreteFilter] =
      Definition[ConcreteFilter](name, params, body)

    @inline def compose(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(ComposeFilters(first, second))

    @inline def silence(f: ConcreteFilter): ConcreteFilter =
      Fix(SilenceExceptions(f))

    @inline def enlist(f: ConcreteFilter): ConcreteFilter =
      Fix(EnlistFilter(f))

    @inline def ensequence(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(EnsequenceFilters(first, second))

    @inline def enject(obj: List[((String Either ConcreteFilter), ConcreteFilter)]): ConcreteFilter =
      Fix(EnjectFilters(obj))

    @inline def call(name: String, params: List[ConcreteFilter] = Nil): ConcreteFilter =
      Fix(CallFilter(name, params))

    @inline def add(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Add))

    @inline def subtract(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Subtract))

    @inline def multiply(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Multiply))

    @inline def divide(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Divide))

    @inline def modulo(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Modulo))

    @inline def equal(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, Equal))

    @inline def lte(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, LTE))

    @inline def gte(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, GTE))

    @inline def lessThan(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, LessThan))

    @inline def greaterThan(first: ConcreteFilter, second: ConcreteFilter): ConcreteFilter =
      Fix(FilterMath(first, second, GreaterThan))


    @inline def not: ConcreteFilter =
      Fix(FilterNot())

    @inline def constNumber(value: Double): ConcreteFilter =
      Fix(ConstNumber(value))

    @inline def constString(value: String): ConcreteFilter =
      Fix(ConstString(value))

    @inline def constBoolean(value: Boolean): ConcreteFilter =
      Fix(ConstBoolean(value))

    @inline def asBinding(name: String, as: ConcreteFilter, in: ConcreteFilter): ConcreteFilter =
      Fix(AsBinding[ConcreteFilter](name, as, in))

    @inline def deref(name: String): ConcreteFilter =
      Fix(Dereference[ConcreteFilter](name))

}
