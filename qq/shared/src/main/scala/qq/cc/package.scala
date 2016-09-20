package qq

import monix.eval.Task
import qq.data.VarBinding

import scalaz.{NonEmptyList, \/}

package object cc {

  type VarBindings[J] = Map[String, VarBinding[J]]
  type CompiledFilter[J] = VarBindings[J] => CompiledProgram[J]
  type CompiledProgram[J] = J => Task[List[J]]
  type OrCompilationError[T] = QQCompilationException \/ T

  implicit class ListToNelOps[A](val l: List[A]) extends AnyVal {
    def toNel: Option[NonEmptyList[A]] = if (l.isEmpty) None else Some(NonEmptyList(l.head, l.tail: _*))
  }

}
