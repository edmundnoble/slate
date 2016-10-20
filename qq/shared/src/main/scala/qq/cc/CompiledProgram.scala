package qq
package cc

import cats.Monoid
import cats.data.{NonEmptyList, ValidatedNel}
import monix.eval.Task
import qq.util.TaskParallel
import qq.util._
import monix.cats._
import org.atnos.eff._, Eff._, syntax.all._
import qq.data.JSON

import cats.implicits._

object CompiledProgram {

  @inline def singleton(f: JSON => Eff[CompiledProgramStack, JSON]): CompiledProgram =
    Arrs.singleton(f)

  @inline def id: CompiledProgram =
    singleton(_.pureEff)

  @inline def const(value: JSON): CompiledProgram =
    singleton(_ => value.pureEff)

  @inline final def composePrograms(f: CompiledProgram, s: CompiledProgram): CompiledProgram =
    Arrs(f.functions ++ s.functions)

  implicit def compiledProgramMonoid: Monoid[CompiledProgram] = new Monoid[CompiledProgram] {
    override def combine(f1: CompiledProgram, f2: CompiledProgram): CompiledProgram =
      composePrograms(f1, f2)

    override def empty: CompiledProgram =
      id
  }


}
