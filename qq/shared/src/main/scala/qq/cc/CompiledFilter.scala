package qq
package cc

import monix.eval.Task
import monix.scalaz._
import qq.data.{JSON, VarBinding}
import qq.util._

import scalaz.syntax.tag._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.apply._
import scalaz.syntax.validation._
import scalaz.{Monoid, NonEmptyList, Validation, ValidationNel}

object CompiledFilter {

  @inline final def id: CompiledFilter =
    _ => j => Task.now((j :: Nil).successNel)

  @inline final def const(value: JSON): CompiledFilter =
    _ => _ => Task.now((value :: Nil).successNel)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    _ => _ => Task.now(values.successNel)

  @inline final def func(f: CompiledProgram): CompiledFilter =
    _ => f

  def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter = bindings =>
    CompiledProgram.composePrograms(f(bindings), s(bindings))

  def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    bindings => jsv => Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv))((f, s) => (f |@| s) {
      _ ++ _
    })

  def zipFiltersWith
  (first: CompiledFilter, second: CompiledFilter, fun: (JSON, JSON) => Task[ValidationNel[QQRuntimeError, JSON]]): CompiledFilter =
    bindings => jsv =>
      Task.mapBoth(first(bindings)(jsv), second(bindings)(jsv)) { (f, s) =>
        val x: ValidationNel[QQRuntimeError, List[Task[ValidationNel[QQRuntimeError, JSON]]]] = (f |@| s) ((fu, fs) => (fu, fs).zipped.map(fun))
        val x2: Task[Validation[NonEmptyList[QQRuntimeError], List[ValidationNel[QQRuntimeError, JSON]]]] = x.traverse(_.traverse[Task, ValidationNel[QQRuntimeError, JSON]](identity))
        val x3: Task[Validation[NonEmptyList[QQRuntimeError], List[JSON]]] = x2.map(_.map(_.traverse[ValidationNel[QQRuntimeError, ?], JSON](identity)).flatten)
        x3
      }.flatten

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter =
    bindings => v => {
      import Validation.FlatMap._
      val res: Task[ValidationNel[QQRuntimeError, List[JSON]]] =
        as(bindings)(v)
      val newBindings: Task[ValidationNel[QQRuntimeError, List[VarBinding]]] =
        res.map(_.map(_.map(VarBinding(name, _))))
      val allBindings: Task[ValidationNel[QQRuntimeError, List[Map[String, VarBinding]]]] =
        newBindings.map(_.map(_.map(b => bindings + (b.name -> b))))
      val apped: Task[ValidationNel[QQRuntimeError, List[Task[ValidationNel[QQRuntimeError, List[JSON]]]]]] =
        allBindings.map(_.map(_.map(in(_)(v))))
      val t: Task[ValidationNel[QQRuntimeError, List[ValidationNel[QQRuntimeError, List[JSON]]]]] =
        apped.flatMap(_.traverse(_.traverse(identity[Task[ValidationNel[QQRuntimeError, List[JSON]]]])))
      val r: Task[ValidationNel[QQRuntimeError, ValidationNel[QQRuntimeError, List[List[JSON]]]]] =
        t.map(_.map(_.traverse[ValidationNel[QQRuntimeError, ?], List[JSON]](identity)))
      val x: Task[ValidationNel[QQRuntimeError, List[JSON]]] =
        r.map(_.flatMap(_.map(_.flatten)))
      x
    }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def append(a: CompiledFilter, b: => CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def zero: CompiledFilter =
      id
  }

}
