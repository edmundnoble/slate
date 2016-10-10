package qq
package cc

import qq.data.CompiledDefinition
import qq.util.Recursion.RecursionEngine

import scalaz.{Monoid, Plus, PlusEmpty}
import scalaz.syntax.either._

object Prelude {
  val empty = new Prelude {
    override def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] = IndexedSeq.empty.right
  }

  implicit val preludeMonoid: Monoid[Prelude] = new Monoid[Prelude] {
    override def zero: Prelude = Prelude.empty

    override def append(f1: Prelude, f2: => Prelude): Prelude = new Prelude {
      override def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]] = for {
        fst <- f1.all
        snd <- f2.all
      } yield fst ++ snd
    }
  }
}

trait Prelude {
  def all(implicit rec: RecursionEngine): OrCompilationError[IndexedSeq[CompiledDefinition]]
}

