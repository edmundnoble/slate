package qq
package cc

import cats.data._
import monix.eval.Task
import monix.cats._
import qq.data.{JSON, VarBinding}
import qq.util._
import cats.{Eval, Monoid, Traverse}
import cats.implicits._
import org.atnos.eff
import org.atnos.eff.{Arrs, Eff, Fx}
import org.atnos.eff.syntax.eff._

object CompiledFilter {

  @inline final def singleton(filt: JSON => Eff[CompiledFilterStack, List[JSON]]): CompiledFilter =
    RanTraverseM.singleton[CompiledFilterStack, List, JSON, JSON](filt)

  @inline final def constE(result: Eff[CompiledFilterStack, List[JSON]]): CompiledFilter =
    RanTraverseM.singleton[CompiledFilterStack, List, JSON, JSON](_ => result)

  @inline final def id: CompiledFilter =
    singleton(j => (j :: Nil).pureEff)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    constE(values.pureEff)

  @inline final def const(value: JSON): CompiledFilter =
    constL(value :: Nil)

  @inline final def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter =
    RanTraverseM.compose(f, s)

  @inline final def ensequenceCompiledFilters
  (first: CompiledFilter, second: CompiledFilter): CompiledFilter =
    singleton(j =>
      (first(j) |@| second(j)).map(_ ++ _)
    )

  def asBinding(name: String, as: CompiledFilter, in: CompiledFilter): CompiledFilter = singleton { (j: JSON) =>
    for {
      bindings <- eff.reader.ask[CompiledFilterStack, VarBindings]
      results <- as.apply(j)
      inRan <- results.traverseA(r => eff.reader.runReader(bindings + (name -> VarBinding(name, r)))(in(j))).into[CompiledFilterStack]
    } yield inRan.flatten
  }

  def runStack[A](bindings: VarBindings, result: CompiledFilterResult[List[A]]): Task[ValidatedNel[QQRuntimeError, List[A]]] = {
    // TODO: investigate the compiler crash that happens without providing these type arguments explicitly
    type mem = eff.Member.Aux[VarEnv, CompiledFilterStack, CompiledProgramStack]
    type mem1 = eff.Member.Aux[OrRuntimeErr, CompiledProgramStack, Fx.fx1[TaskParallel]]
    val read: Eff[CompiledProgramStack, List[A]] =
      eff.reader.runReader[CompiledFilterStack, CompiledProgramStack, VarBindings, List[A]](bindings)(result)(implicitly[mem])
    val erred: Eff[Fx.fx1[TaskParallel], OrRuntimeErr[List[A]]] =
      validated
        .by[NonEmptyList[QQRuntimeError]]
        .runErrorParallel[Fx.fx2[TaskParallel, OrRuntimeErr], Fx.fx1[TaskParallel], List[A]](read)(implicitly[mem1])

    Eff.detachA[TaskParallel, ValidatedNel[QQRuntimeError, List[A]]](erred).unwrap
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
