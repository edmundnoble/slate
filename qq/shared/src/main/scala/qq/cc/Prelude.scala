package qq
package cc

import cats.implicits._
import cats.Monoid
import qq.data.CompiledDefinition
import qq.util.Recursion.RecursionEngine


object Prelude {
  def empty[C] = new Prelude[C] {
    override def all(runtime: QQRuntime[C])(implicit rec: RecursionEngine): OrCompilationError[Vector[C]] =
      Right(Vector.empty)
  }

  implicit def preludeMonoid[C]: Monoid[Prelude[C]] = new Monoid[Prelude[C]] {
    override def empty: Prelude[C] = Prelude.empty

    override def combine(f1: Prelude[C], f2: Prelude[C]): Prelude[C] = new Prelude[C] {
      override def all(runtime: QQRuntime[C])(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition[C]]] = for {
        fst <- f1.all(runtime)
        snd <- f2.all(runtime)
      } yield fst ++ snd
    }
  }
}

trait Prelude[C] {
  def all(runtime: QQRuntime[C])(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition[C]]]
}

