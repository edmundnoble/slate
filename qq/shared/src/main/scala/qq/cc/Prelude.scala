package qq
package cc

import qq.data.CompiledDefinition

import scalaz.Monoid
import scalaz.syntax.either._

object Prelude {
  implicit def preludeMonoid[J]: Monoid[Prelude[J]] = new Monoid[Prelude[J]] {
    override def zero: Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] = IndexedSeq.empty.right
    }

    override def append(f1: Prelude[J], f2: => Prelude[J]): Prelude[J] = new Prelude[J] {
      override def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]] = for {
        fst <- f1.all(runtime)
        snd <- f2.all(runtime)
      } yield fst ++ snd
    }
  }
}

trait Prelude[J] {
  def all(runtime: QQRuntime[J]): OrCompilationError[IndexedSeq[CompiledDefinition[J]]]
}

