package qq
package cc

import cats.data._
import monix.eval.Task
import monix.cats._
import qq.data.{JSON, VarBinding}
import qq.util._
import cats.{Monoid, Traverse}
import cats.implicits._
import org.atnos.eff
import org.atnos.eff.{Arrs, Eff, Fx}
import org.atnos.eff.syntax.eff._

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
    constE(Eff.send(values))

  @inline final def func(f: CompiledProgram): CompiledFilter =
    singleton(f(_).into)

  @inline final def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter =
    Arrs(f.functions ++ s.functions)

  @inline final def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    singleton(j =>
      Eff.collapse(eff.list.runList(second(j)).ap(eff.list.runList(first(j)).map(l => (l2: List[JSON]) => l ++ l2)).into[CompiledFilterStack])
    )

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter = singleton { (j: JSON) =>
    for {
      bindings <- eff.reader.ask[CompiledFilterStack, VarBindings]
      result <- as.apply(j)
      inRan <- eff.reader.runReader(bindings + (name -> VarBinding(name, result)))(in(j)).into[CompiledFilterStack]
    } yield inRan
  }

  def runStack[A](bindings: VarBindings, result: CompiledFilterResult[A]): Task[ValidatedNel[QQRuntimeError, List[A]]] = {
    type mem = eff.Member.Aux[VarEnv, CompiledFilterStack, CompiledProgramStack]
    type mem2 = eff.Member.Aux[List, CompiledProgramStack, Fx.fx2[Task, OrRuntimeErr]]
    type mem3 = eff.Member.Aux[OrRuntimeErr, Fx.fx2[Task, OrRuntimeErr], Fx.fx1[Task]]
    val read: Eff[CompiledProgramStack, A] =
      eff.reader.runReader[CompiledFilterStack, CompiledProgramStack, VarBindings, A](bindings)(result)(implicitly[mem])
    val listed: Eff[Fx.fx2[Task, OrRuntimeErr], List[A]] =
      eff.list.runList[CompiledProgramStack, Fx.fx2[Task, OrRuntimeErr], A](read)(implicitly[mem2])
    val erred: Eff[Fx.fx1[Task], Validated[NonEmptyList[QQRuntimeError], List[A]]] =
      validated
        .by[NonEmptyList[QQRuntimeError]]
        .runErrorParallel[Fx.fx2[Task, OrRuntimeErr], Fx.fx1[Task], List[A]](listed)(implicitly[mem3])

    Eff.detachA[Task, ValidatedNel[QQRuntimeError, List[A]]](erred)
  }

  def run(in: JSON, bindings: VarBindings, filter: CompiledFilter): Task[ValidatedNel[QQRuntimeError, List[JSON]]] = {
    runStack(bindings, filter(in))
  }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def combine(a: CompiledFilter, b: CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def empty: CompiledFilter =
      id
  }

}
