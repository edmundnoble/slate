package qq
package cc

import monix.eval.Task
import qq.util.TaskParallel
import qq.util._
import monix.scalaz._
import qq.data.JSON

import scalaz.{Monoid, NonEmptyList, Validation, ValidationNel}
import scalaz.syntax.traverse._
import scalaz.syntax.tag._
import scalaz.std.list._
import scalaz.syntax.validation._

object CompiledProgram {

  @inline def id: CompiledProgram =
    (j: JSON) => Task.now((j :: Nil).successNel)

  @inline def const(value: JSON): CompiledProgram =
    (_: JSON) => Task.now((value :: Nil).successNel)

  @inline final def composePrograms(f: CompiledProgram, s: CompiledProgram): CompiledProgram = { (j: JSON) =>
    val re: Task[Validation[NonEmptyList[QQRuntimeError], List[JSON]]] =
      f(j).flatMap { e =>
        val r: Task[Validation[NonEmptyList[QQRuntimeError], Validation[NonEmptyList[QQRuntimeError], List[JSON]]]] =
          e.traverse[Task, NonEmptyList[QQRuntimeError], Validation[NonEmptyList[QQRuntimeError], List[JSON]]] { l =>
            val r2: Task[List[ValidationNel[QQRuntimeError, List[JSON]]]] = l.traverse[Task, ValidationNel[QQRuntimeError, List[JSON]]](s)
            val r3: Task[ValidationNel[QQRuntimeError, List[List[JSON]]]] = r2.map(_.sequence[ValidationNel[QQRuntimeError, ?], List[JSON]])
            r3.map(_.map(_.flatten))
          }
        r.map(_.flatten)
      }
    re
  }

  implicit def compiledProgramMonoid: Monoid[CompiledProgram] = new Monoid[CompiledProgram] {
    override def append(f1: CompiledProgram, f2: => CompiledProgram): CompiledProgram =
      composePrograms(f1, f2)

    override def zero: CompiledProgram =
      id
  }


}
