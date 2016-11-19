package qq
package cc

import cats.data._
import cats.implicits._
import cats.{Monoid, Semigroup}
import monix.eval.Task
import org.atnos.eff
import org.atnos.eff.syntax.eff._
import org.atnos.eff.{Eff, Fx}
import qq.data.{JSON, VarBinding}
import qq.util._

// Tools for CompiledFilters; that is, QQ filters that have been compiled.
// QQ filters, after compilation, are functions from JSON to Eff[CompiledFilterStack, List[JSON]]
// (with a special representation to avoid stack overflows, see FlatTraverseArrs.scala)
// CompiledFilterStack is all of the effects that can be associated with the result of a CompiledFilter's execution:
// 1. It has an environment of variables it can access (VarEnv)
// 2. It can return errors (OrRuntimeErr)
// 3. It can perform asynchronous side-effects (TaskParallel)
object CompiledFilter {

  @inline final def singleton(filt: JSON => Eff[CompiledFilterStack, List[JSON]]): CompiledFilter =
    FlatTraverseArrs.singleton[CompiledFilterStack, List, JSON, JSON](filt)

  @inline final def constE(result: Eff[CompiledFilterStack, List[JSON]]): CompiledFilter =
    FlatTraverseArrs.singleton[CompiledFilterStack, List, JSON, JSON](_ => result)

  @inline final def id: CompiledFilter =
    singleton(j => (j :: Nil).pureEff)

  @inline final def constL(values: List[JSON]): CompiledFilter =
    constE(values.pureEff)

  @inline final def const(value: JSON): CompiledFilter =
    constL(value :: Nil)

  @inline final def composeFilters(f: CompiledFilter, s: CompiledFilter): CompiledFilter =
    FlatTraverseArrs.compose(f, s)

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

  def runStack[A](bindings: VarBindings, result: CompiledFilterResult[List[A]]): Task[Either[NonEmptyList[QQRuntimeError], List[A]]] = {
    // TODO: investigate the compiler crash that happens without providing these type arguments explicitly
    type mem = eff.Member.Aux[VarEnv, CompiledFilterStack, CompiledProgramStack]
    type mem1 = eff.Member.Aux[OrRuntimeErr, CompiledProgramStack, Fx.fx1[TaskParallel]]
    val read: Eff[CompiledProgramStack, List[A]] =
      eff.reader.runReader[CompiledFilterStack, CompiledProgramStack, VarBindings, List[A]](bindings)(result)(implicitly[mem])
    val erred: Eff[Fx.fx1[TaskParallel], OrRuntimeErr[List[A]]] =
      eff.either
        .runEitherCombine[Fx.fx2[TaskParallel, OrRuntimeErr], Fx.fx1[TaskParallel], NonEmptyList[QQRuntimeError], List[A]](read)(implicitly[mem1], implicitly[Semigroup[RuntimeErrs]])

    Eff.detachA[TaskParallel, Either[NonEmptyList[QQRuntimeError], List[A]]](erred)(util.TaskParMonad, util.TaskParAp).unwrap
  }

  def run(in: JSON, bindings: VarBindings, filter: CompiledFilter): Task[Either[NonEmptyList[QQRuntimeError], List[JSON]]] = {
    runStack(bindings, filter(in))
  }

  implicit def compiledFilterMonoid: Monoid[CompiledFilter] = new Monoid[CompiledFilter] {
    override def combine(a: CompiledFilter, b: CompiledFilter): CompiledFilter =
      composeFilters(a, b)

    override def empty: CompiledFilter =
      id
  }

}
