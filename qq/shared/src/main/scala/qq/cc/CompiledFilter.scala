package qq
package cc

import monix.eval.Task
import monix.scalaz._
import qq.data.{JSON, VarBinding}
import qq.util._

import scalaz.syntax.tag._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.Monoid

object CompiledFilter {

  @inline final def id: CompiledFilter =
    _ => j => Task.now(j :: Nil)

  @inline final def const(value: JSON): CompiledFilter =
    _ => _ => Task.now(value :: Nil)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    _ => _ => Task.now(values)

  @inline final def func(f: CompiledProgram): CompiledFilter =
    _ => f

  def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter = bindings =>
    f(bindings).andThen(_.flatMap(_.traverseM[TaskParallel, JSON](s(bindings).andThen(_.parallel)).unwrap))

  def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    bindings => jsv => Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv))(_ ++ _)

  def zipFiltersWith
  (first: CompiledFilter, second: CompiledFilter, fun: (JSON, JSON) => Task[JSON]): CompiledFilter =
    bindings => jsv =>
      Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv)) { (f, s) => (f, s).zipped.map(fun) }.map(_.sequence).flatten

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter = {
    (bindings: VarBindings) =>
      (v: JSON) =>
        val res = as(bindings)(v)
        val newBindings = res.map(_.map(VarBinding(name, _)))
        val allBindings = newBindings.map(_.map(b => bindings + (b.name -> b)))
        allBindings.map(_.map(in(_)(v))).flatMap(_.sequence[Task, List[JSON]].map(_.flatten))
  }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def append(a: CompiledFilter, b: => CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def zero: CompiledFilter =
      id
  }

}
