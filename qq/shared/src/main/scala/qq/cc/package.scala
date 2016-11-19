package qq

import cats.data._
import monix.eval.Task
import org.atnos.eff.Eff._
import org.atnos.eff._
import qq.cc.FlatTraverseArrs._taskPar
import qq.data.{JSON, VarBinding}
import qq.util._

package object cc {

  type VarBindings = Map[String, VarBinding]
  type VarEnv[A] = Reader[VarBindings, A]
  type RuntimeErrs = NonEmptyList[QQRuntimeError]
  type OrRuntimeErr[+A] = Either[RuntimeErrs, A]
  type _orRuntimeErr[R] = Member[Either[RuntimeErrs, ?], R]

  type CompiledMathOperator = (JSON, JSON) => Either[RuntimeErrs, JSON]
  type CompiledFilterStack = Fx.fx3[VarEnv, TaskParallel, OrRuntimeErr]
  type CompiledProgramStack = Fx.fx2[TaskParallel, OrRuntimeErr]
  type CompiledFilterResult[A] = Eff[CompiledFilterStack, A]
  type CompiledFilter = FlatTraverseArrs[CompiledFilterStack, List, JSON, JSON]

  type OrCompilationError[T] = QQCompilationException Either T

  implicit final class ListToNelOps[A](val l: List[A]) extends AnyVal {
    @inline def unconsFold[B](b: B, f: (A, List[A]) => B): B = if (l.isEmpty) b else f(l.head, l.tail)
    @inline def nelFoldLeft1(ifEmpty: A)(foldFun: (A, A) => A): A = if (l.isEmpty) ifEmpty else l.reduceLeft(foldFun)
  }

  // TODO: remove when added to eff-cats
  def suspend[R: _taskPar, A](task: => TaskParallel[Eff[R, A]]): Eff[R, A] =
    send[TaskParallel, R, Eff[R, A]](Task.suspend(task).parallel).flatten

}
