package qq

import cats.data.{ValidatedNel, Xor}
import monix.eval.Task
import qq.data.{JSON, VarBinding}

package object cc {

  type VarBindings = Map[String, VarBinding]
  type CompiledFilter = VarBindings => CompiledProgram
  type CompiledMathOperator = (JSON, JSON) => ValidatedNel[QQRuntimeError, JSON]
  type CompiledProgram = JSON => Task[ValidatedNel[QQRuntimeError, List[JSON]]]

  type OrCompilationError[T] = QQCompilationException Xor T

  implicit final class ListToNelOps[A](val l: List[A]) extends AnyVal {
    @inline def unconsFold[B](b: B, f: (A, List[A]) => B): B = if (l.isEmpty) b else f(l.head, l.tail)
    @inline def nelFoldLeft1(ifEmpty: A)(foldFun: (A, A) => A): A = if (l.isEmpty) ifEmpty else l.reduceLeft(foldFun)
  }

}
