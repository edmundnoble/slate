package qq
package cc

import cats.data._
import cats.implicits._
import cats.{Monoid, Semigroup}
import monix.eval.Task
import org.atnos.eff
import org.atnos.eff.syntax.eff._
import org.atnos.eff.{Eff, Fx}
import qq.data.JSON
import qq.util._

// Tools for InterpretedFilters; that is, QQ filters that have been compiled.
// QQ filters, after compilation, are functions from JSON to Eff[InterpretedFilterStack, Vector[JSON]]
// (with a special representation to avoid stack overflows, see FlatTraverseArrs.scala)
// InterpretedFilterStack is all of the effects that can be associated with the result of a InterpretedFilter's execution:
// 1. It has an environment of variables it can access (VarEnv)
// 2. It can return errors (OrRuntimeErr)
// 3. It can perform asynchronous side-effects (TaskParallel)
object InterpretedFilter {

  @inline final def singleton(filt: JSON => Eff[InterpretedFilterStack, Vector[JSON]]): InterpretedFilter =
    FlatTraverseArrs.singleton[InterpretedFilterStack, Vector, JSON, JSON](filt)

  @inline final def constE(result: Eff[InterpretedFilterStack, Vector[JSON]]): InterpretedFilter =
    FlatTraverseArrs.singleton[InterpretedFilterStack, Vector, JSON, JSON](_ => result)

  @inline final def id: InterpretedFilter =
    singleton(j => (j +: Vector.empty).pureEff)

  @inline final def constL(values: Vector[JSON]): InterpretedFilter =
    constE(values.pureEff)

  @inline final def const(value: JSON): InterpretedFilter =
    constL(value +: Vector.empty)

  @inline final def composeFilters(f: InterpretedFilter, s: InterpretedFilter): InterpretedFilter =
    FlatTraverseArrs.compose(f, s)

  @inline final def ensequence
  (first: InterpretedFilter, second: InterpretedFilter): InterpretedFilter =
    singleton(j =>
      (first(j) |@| second(j)).map(_ ++ _)
    )

  def runStack[A](bindings: VarBindings, result: Eff[InterpretedFilterStack, Vector[A]]): Task[Either[NonEmptyList[QQRuntimeError], Vector[A]]] = {
    // TODO: investigate the compiler crash that happens without providing these type arguments explicitly
    type mem = eff.Member.Aux[VarEnv, InterpretedFilterStack, InterpretedProgramStack]
    type mem1 = eff.Member.Aux[OrRuntimeErr, InterpretedProgramStack, Fx.fx1[TaskParallel]]
    val read: Eff[InterpretedProgramStack, Vector[A]] =
      eff.reader.runReader[InterpretedFilterStack, InterpretedProgramStack, VarBindings, Vector[A]](bindings)(result)(implicitly[mem])
    val erred: Eff[Fx.fx1[TaskParallel], OrRuntimeErr[Vector[A]]] =
      eff.either
        .runEitherCombine[Fx.fx2[TaskParallel, OrRuntimeErr], Fx.fx1[TaskParallel], NonEmptyList[QQRuntimeError], Vector[A]](read)(implicitly[mem1], implicitly[Semigroup[RuntimeErrs]])

    Eff.detachA[TaskParallel, Either[NonEmptyList[QQRuntimeError], Vector[A]]](erred)(util.TaskParMonad, util.TaskParAp).unwrap
  }

  def run(in: JSON, bindings: VarBindings, filter: InterpretedFilter): Task[Either[NonEmptyList[QQRuntimeError], Vector[JSON]]] = {
    runStack(bindings, filter(in))
  }

  implicit def compiledFilterMonoid: Monoid[InterpretedFilter] = new Monoid[InterpretedFilter] {
    override def combine(a: InterpretedFilter, b: InterpretedFilter): InterpretedFilter =
      composeFilters(a, b)

    override def empty: InterpretedFilter =
      id
  }

}
