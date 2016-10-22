package qq
package cc

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import monix.eval.Task
import monix.cats._
import qq.data.{JSON, VarBinding}
import qq.util._
import cats.Monoid
import cats.implicits._
import org.atnos.eff._, Eff._, syntax.all._

object CompiledFilter {

  @inline final def singleton(filt: JSON => Eff[CompiledFilterStack, JSON]): Arrs[CompiledFilterStack, JSON, JSON] =
    Arrs.singleton(filt)

  @inline final def constE(result: Eff[CompiledFilterStack, JSON]): Arrs[CompiledFilterStack, JSON, JSON] =
    Arrs.singleton(_ => result)

  @inline final def id: CompiledFilter =
    singleton(_.pureEff)

  @inline final def const(value: JSON): CompiledFilter =
    singleton(_ => value.pureEff)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    constE(values.send)

  @inline final def func(f: CompiledProgram): CompiledFilter =
    singleton(f(_).into)

  @inline final def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter =
    Arrs(f.functions ++ s.functions)

  @inline final def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    singleton(j =>
      Eff.collapse(list.runList(second(j)).ap(list.runList(first(j)).map(l => (l2: List[JSON]) => l ++ l2)).into[CompiledFilterStack])
    )

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter = singleton { (j: JSON) =>
    for {
      bindings <- reader.ask[CompiledFilterStack, VarBindings]
      result <- as.apply(j)
      inRan <- reader.runReader(bindings + (name -> VarBinding(name, result)))(in(j)).into[CompiledFilterStack]
    } yield inRan
  }

  def run(in: JSON, bindings: VarBindings, filter: CompiledFilter): Task[ValidatedNel[QQRuntimeError, List[JSON]]] = {
    runStack(bindings, filter(in))
  }

  def runStack[A](bindings: VarBindings, result: CompiledFilterResult[A]): Task[ValidatedNel[QQRuntimeError, List[A]]] = {
    detachA[Task, ValidatedNel[QQRuntimeError, List[A]]](
      validated
        .by[NonEmptyList[QQRuntimeError]]
        .runErrorParallel[Fx.fx2[ValidatedNel[QQRuntimeError, ?], Task], Fx.fx1[Task], List[A]](
        ???
//        list.runList[CompiledProgramStack, Fx.fx2[ValidatedNel[QQRuntimeError, ?], Task], A](
//          reader.runReader[CompiledFilterStack, CompiledProgramStack, VarBindings, A](bindings)(result)
//        )
      )
    )
  }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def combine(a: CompiledFilter, b: CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def empty: CompiledFilter =
      id
  }

}
