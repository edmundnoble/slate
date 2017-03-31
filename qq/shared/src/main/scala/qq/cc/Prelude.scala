package qq
package cc

import cats.Monoid
import org.atnos.eff.{Eff, Fx, list}
import qq.ast.QQRuntime
import qq.cc.CompileError.OrCompileError
import qq.data.CompiledDefinition

object Prelude {
  def empty[C]: Prelude[C] =
    _ => Vector.empty

  implicit def preludeMonoid[C]: Monoid[Prelude[C]] = new Monoid[Prelude[C]] {
    def empty: Prelude[C] = Prelude.empty

    def combine(f1: Prelude[C], f2: Prelude[C]): Prelude[C] =
      runtime => f1.all(runtime) ++ f2.all(runtime)
  }
}

trait Prelude[C] {
  def all(runtime: QQRuntime[C]): Vector[CompiledDefinition[C]]
}

