package qq
package cc

import cats.Monoid
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import qq.data.JSON
import qq.util.{TaskParallel, _}

object CompiledProgram {

  @inline def singleton(f: JSON => Eff[CompiledProgramStack, List[JSON]]): CompiledProgram =
    Arrs.singleton(f)

  @inline def id: CompiledProgram =
    singleton(j => (j :: Nil).pureEff)

  @inline def const(value: JSON): CompiledProgram =
    singleton(_ => (value :: Nil).pureEff)

  @inline final def composePrograms(f: CompiledProgram, s: CompiledProgram): CompiledProgram =
    f.mapLast(_.flatMap(_.traverseA[CompiledProgramStack, List[JSON]](s(_))).map(_.flatten))

  implicit def compiledProgramMonoid: Monoid[CompiledProgram] = new Monoid[CompiledProgram] {
    override def combine(f1: CompiledProgram, f2: CompiledProgram): CompiledProgram =
      composePrograms(f1, f2)

    override def empty: CompiledProgram =
      id
  }


}
