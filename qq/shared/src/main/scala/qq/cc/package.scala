package qq

import cats.{Eval, Traverse}
import cats.data._
import cats.implicits._
import monix.eval.Task
import qq.data.{JSON, VarBinding}
import org.atnos.eff._
import org.atnos.eff.Eff._

package object cc {

  type VarBindings = Map[String, VarBinding]
  type VarEnv[A] = Reader[VarBindings, A]
  type OrRuntimeErr[+A] = ValidatedNel[QQRuntimeError, A]

  type CompiledMathOperator = (JSON, JSON) => ValidatedNel[QQRuntimeError, JSON]
  type CompiledProgramStack = Fx.fx3[Task, OrRuntimeErr, List]
  type CompiledProgramResult[A] = Eff[CompiledProgramStack, A]
  type CompiledProgram = Arrs[CompiledProgramStack, JSON, JSON]
  type CompiledFilterStack = Fx.append[Fx.fx1[VarEnv], CompiledProgramStack]
  type CompiledFilterResult[A] = Eff[CompiledFilterStack, A]
  type CompiledFilter = Arrs[CompiledFilterStack, JSON, JSON]

  type OrCompilationError[T] = QQCompilationException Xor T

  implicit final class ListToNelOps[A](val l: List[A]) extends AnyVal {
    @inline def unconsFold[B](b: B, f: (A, List[A]) => B): B = if (l.isEmpty) b else f(l.head, l.tail)
    @inline def nelFoldLeft1(ifEmpty: A)(foldFun: (A, A) => A): A = if (l.isEmpty) ifEmpty else l.reduceLeft(foldFun)
  }

}
