package qq.cc

import monix.eval.Task
import qq.util.TaskParallel
import qq.util._
import monix.scalaz._
import scalaz.syntax.traverse._
import scalaz.syntax.tag._
import scalaz.std.list._

object CompiledProgram {

  @inline final def id[J]: CompiledProgram[J] =
    (j: J) => Task.now(j :: Nil)

  @inline final def const[J](value: J): CompiledProgram[J] =
    (_: J) => Task.now(value :: Nil)

  def composePrograms[J](f: CompiledProgram[J], s: CompiledProgram[J]): CompiledProgram[J] = {
    f.andThen(_.flatMap(_.traverseM[TaskParallel, J](s.andThen(_.parallel)).unwrap))
  }


}
