package qq

import cats.{Monad, Traverse}
import cats.data._
import monix.eval.Task
import org.atnos.eff.Eff._
import org.atnos.eff._
import qq.cc.FlatTraverseArrs._taskPar
import qq.data.{CompiledDefinition, JSON, VarBinding}
import qq.util._

package object cc {

  type VarBindings = Map[String, VarBinding]
  type VarEnv[A] = Reader[VarBindings, A]
  type RuntimeErrs = NonEmptyList[QQRuntimeError]
  type OrRuntimeErr[+A] = Either[RuntimeErrs, A]
  type OrRuntimeErrs[+A] = Validated[RuntimeErrs, A]
  type _runtimeErr[R] = Member[OrRuntimeErr, R]

  type InterpretedMathOperator = (JSON, JSON) => Either[RuntimeErrs, JSON]
  type InterpretedFilterStack = Fx.fx3[VarEnv, TaskParallel, OrRuntimeErr]
  type InterpretedProgramStack = Fx.fx2[TaskParallel, OrRuntimeErr]
  type InterpretedFilter = FlatTraverseArrs[InterpretedFilterStack, Vector, JSON, JSON]
  type FilterFun = JSON => Eff[InterpretedFilterStack, Vector[JSON]]

  type DefinitionMap[C] = Map[String, CompiledDefinition[C]]

  implicit final class VectorToNelOps[A](val l: Vector[A]) extends AnyVal {
    @inline def unconsFold[B](b: B, f: (A, Vector[A]) => B): B = if (l.isEmpty) b else f(l.head, l.tail)
    @inline def nelFoldLeft1(ifEmpty: A)(foldFun: (A, A) => A): A = if (l.isEmpty) ifEmpty else l.reduceLeft(foldFun)
  }

  final def composeFilterFuns[T[_] : Traverse : Monad, R, I](filterFuns: Vector[I => Eff[R, T[I]]]): I => Eff[R, T[I]] = json =>
    filterFuns.foldLeft(Eff.pure[R, T[I]](Monad[T].pure(json))) { (b, a) =>
      b.flatMap(Eff.flatTraverseA[R, T, I, I](_)(a))
    }

  // TODO: remove when added to eff-cats
  final def suspend[R: _taskPar, A](task: => TaskParallel[Eff[R, A]]): Eff[R, A] =
    send[TaskParallel, R, Eff[R, A]](Task.suspend(task).parallel).flatten

}
