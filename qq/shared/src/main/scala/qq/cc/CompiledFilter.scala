package qq
package cc

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import monix.eval.Task
import monix.cats._
import qq.data.{JSON, VarBinding}
import qq.util._
import cats.Monoid
import cats.implicits._

object CompiledFilter {

  @inline final def id: CompiledFilter =
    _ => j => Task.now((j :: Nil).validNel)

  @inline final def const(value: JSON): CompiledFilter =
    _ => _ => Task.now((value :: Nil).validNel)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    _ => _ => Task.now(values.validNel)

  @inline final def func(f: CompiledProgram): CompiledFilter =
    _ => f

  def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter = bindings =>
    CompiledProgram.composePrograms(f(bindings), s(bindings))

  def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    bindings => jsv => Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv))((f, s) => (f |@| s).map (_ ++ _))

  def zipFiltersWith
  (first: CompiledFilter, second: CompiledFilter, fun: (JSON, JSON) => Task[ValidatedNel[QQRuntimeError, JSON]]): CompiledFilter =
    bindings => jsv =>
      Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv)) { (f, s) =>
        val x: ValidatedNel[QQRuntimeError, List[Task[ValidatedNel[QQRuntimeError, JSON]]]] = (f |@| s).map ((fu, fs) => (fu, fs).zipped.map(fun))
        val x2: Task[Validated[NonEmptyList[QQRuntimeError], List[ValidatedNel[QQRuntimeError, JSON]]]] = x.traverse(_.traverse[Task, ValidatedNel[QQRuntimeError, JSON]](identity))
        val x3: Task[Validated[NonEmptyList[QQRuntimeError], List[JSON]]] = x2.map(_.map(_.traverse[ValidatedNel[QQRuntimeError, ?], JSON](identity)).flatten)
        x3
      }.flatten

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter =
    bindings => v => {
      val res: Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
        as(bindings)(v)
      val newBindings: Task[ValidatedNel[QQRuntimeError, List[VarBinding]]] =
        res.map(_.map(_.map(VarBinding(name, _))))
      val allBindings: Task[ValidatedNel[QQRuntimeError, List[Map[String, VarBinding]]]] =
        newBindings.map(_.map(_.map(b => bindings + (b.name -> b))))
      val apped: Task[ValidatedNel[QQRuntimeError, List[Task[ValidatedNel[QQRuntimeError, List[JSON]]]]]] =
        allBindings.map(_.map(_.map(in(_)(v))))
      val t: Task[ValidatedNel[QQRuntimeError, List[ValidatedNel[QQRuntimeError, List[JSON]]]]] =
        apped.flatMap(_.traverse(_.traverse(identity[Task[ValidatedNel[QQRuntimeError, List[JSON]]]])))
      val r: Task[ValidatedNel[QQRuntimeError, ValidatedNel[QQRuntimeError, List[List[JSON]]]]] =
        t.map(_.map(_.traverse[ValidatedNel[QQRuntimeError, ?], List[JSON]](identity)))
      val x: Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
        r.map(_.flatMap(_.map(_.flatten)))
      x
    }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def combine(a: CompiledFilter, b: CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def empty: CompiledFilter =
      id
  }

}
