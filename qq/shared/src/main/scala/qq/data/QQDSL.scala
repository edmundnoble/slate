package qq
package data

import qq.data.ast._
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

  implicit final class extraOps(val f: FilterAST) {
    def |(next: FilterAST): FilterAST = compose(f, next)
    def |+|(next: FilterAST): FilterAST = ensequence(f, next)
    def +(next: FilterAST): FilterAST = add(f, next)
    def ===(next: FilterAST): FilterAST = equal(f, next)
    def =!=(next: FilterAST): FilterAST = equal(f, next) | not
    def <==(next: FilterAST): FilterAST = lte(f, next)
    def >==(next: FilterAST): FilterAST = gte(f, next)
    def <(next: FilterAST): FilterAST = lessThan(f, next)
    def >(next: FilterAST): FilterAST = greaterThan(f, next)
    def -(next: FilterAST): FilterAST = subtract(f, next)
    def *(next: FilterAST): FilterAST = multiply(f, next)
    def /(next: FilterAST): FilterAST = divide(f, next)
    def %(next: FilterAST): FilterAST = modulo(f, next)
  }

  implicit def embedStr(s: String): FilterAST = constString(s)

  implicit def embedInt(d: Int): FilterAST = constNumber(d)

  implicit def embedDouble(d: Double): FilterAST = constNumber(d)

  @inline def id: FilterAST =
    getPath(Vector.empty)

  @inline implicit def getPath(components: Vector[PathComponent]): FilterAST = {
    FilterComponent.embed(PathOperation(components, PathGet))
  }

  @inline implicit def getPathS(component: PathComponent): FilterAST = {
    getPath(component +: Vector.empty)
  }

  @inline def setPath(components: Vector[PathComponent], value: FilterAST): FilterAST = {
    Fix(PathOperation(components, PathSet(value)))
  }

  @inline def modifyPath(components: Vector[PathComponent], modify: FilterAST): FilterAST = {
    Fix(PathOperation(components, PathModify(modify)))
  }

  @inline def define(name: String, params: Vector[String], body: FilterAST): Definition[FilterAST] =
    Definition[FilterAST](name, params, body)

  @inline def compose(first: FilterAST, second: FilterAST): FilterAST =
    Fix(ComposeFilters(first, second))

  @inline def silence(f: FilterAST): FilterAST =
    Fix(SilenceExceptions(f))

  @inline def enlist(f: FilterAST): FilterAST =
    Fix(EnlistFilter(f))

  @inline def ensequence(first: FilterAST, second: FilterAST): FilterAST =
    Fix(EnsequenceFilters(first, second))

  @inline def enject(obj: Vector[((String Either FilterAST), FilterAST)]): FilterAST =
    Fix(EnjectFilters(obj))

  @inline def call(name: String, params: Vector[FilterAST] = Vector.empty): FilterAST =
    Fix(CallFilter(name, params))

  @inline def add(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Add))

  @inline def subtract(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Subtract))

  @inline def multiply(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Multiply))

  @inline def divide(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Divide))

  @inline def modulo(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Modulo))

  @inline def equal(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, Equal))

  @inline def lte(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, LTE))

  @inline def gte(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, GTE))

  @inline def lessThan(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, LessThan))

  @inline def greaterThan(first: FilterAST, second: FilterAST): FilterAST =
    Fix(FilterMath(first, second, GreaterThan))

  @inline def not: FilterAST =
    FilterComponent.embed(FilterNot())

  @inline def constNumber(value: Double): FilterAST =
    FilterComponent.embed(ConstNumber(value))

  @inline def constString(value: String): FilterAST =
    FilterComponent.embed(ConstString(value))

  @inline def constBoolean(value: Boolean): FilterAST =
    FilterComponent.embed(ConstBoolean(value))

  @inline def asBinding(name: String, as: FilterAST, in: FilterAST): FilterAST =
    Fix(AsBinding[FilterAST](name, as, in))

  @inline def deref(name: String): FilterAST =
    Fix(Dereference[FilterAST](name))

}
