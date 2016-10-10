package qq
package cc

import monix.eval.Task
import qq.util.TaskParallel
import qq.util._
import monix.scalaz._
import qq.data.JSON

import scalaz.Monoid
import scalaz.syntax.traverse._
import scalaz.syntax.tag._
import scalaz.std.list._

object CompiledProgram {

  @inline def id: CompiledProgram =
    (j: JSON) => Task.now(j :: Nil)

  @inline def const(value: JSON): CompiledProgram =
    (_: JSON) => Task.now(value :: Nil)

  @inline final def composePrograms(f: CompiledProgram, s: CompiledProgram): CompiledProgram =
    f.andThen(_.flatMap(_.traverseM[TaskParallel, JSON](s.andThen(_.parallel)).unwrap))

  implicit def compiledProgramMonoid: Monoid[CompiledProgram] = new Monoid[CompiledProgram] {
    override def append(f1: CompiledProgram, f2: => CompiledProgram): CompiledProgram =
      composePrograms(f1, f2)

    override def zero: CompiledProgram =
      id
  }


}
