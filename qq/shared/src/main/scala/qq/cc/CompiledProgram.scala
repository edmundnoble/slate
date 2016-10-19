package qq
package cc

import cats.Monoid
import cats.data.{NonEmptyList, ValidatedNel}
import monix.eval.Task
import qq.util.TaskParallel
import qq.util._
import monix.cats._
import qq.data.JSON

import cats.implicits._

object CompiledProgram {

  @inline def id: CompiledProgram =
    (j: JSON) => Task.now((j :: Nil).validNel)

  @inline def const(value: JSON): CompiledProgram =
    (_: JSON) => Task.now((value :: Nil).validNel)

  @inline final def composePrograms(f: CompiledProgram, s: CompiledProgram): CompiledProgram = { (j: JSON) =>
    val re: Task[ValidatedNel[QQRuntimeError, List[JSON]]] =
      f(j).flatMap { e =>
        val r: Task[ValidatedNel[QQRuntimeError, ValidatedNel[QQRuntimeError, List[JSON]]]] =
          e.traverse[Task, NonEmptyList[QQRuntimeError], ValidatedNel[QQRuntimeError, List[JSON]]] { l =>
            val r2: Task[List[ValidatedNel[QQRuntimeError, List[JSON]]]] = l.traverse[Task, ValidatedNel[QQRuntimeError, List[JSON]]](s)
            val r3: Task[ValidatedNel[QQRuntimeError, List[List[JSON]]]] = r2.map(_.sequence[ValidatedNel[QQRuntimeError, ?], List[JSON]])
            r3.map(_.map(_.flatten))
          }
        r.map(_.flatten)
      }
    re
  }

  implicit def compiledProgramMonoid: Monoid[CompiledProgram] = new Monoid[CompiledProgram] {
    override def combine(f1: CompiledProgram, f2: CompiledProgram): CompiledProgram =
      composePrograms(f1, f2)

    override def empty: CompiledProgram =
      id
  }


}
