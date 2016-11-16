package qq
package cc

import qq.data.CompiledDefinition
import qq.util.Recursion.RecursionEngine
import cats.implicits._
import cats.Monoid


object Prelude {
  val empty = new Prelude {
    override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] =
      Right(Vector.empty)
  }

  implicit val preludeMonoid: Monoid[Prelude] = new Monoid[Prelude] {
    override def empty: Prelude = Prelude.empty

    override def combine(f1: Prelude, f2: Prelude): Prelude = new Prelude {
      override def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]] = for {
        fst <- f1.all
        snd <- f2.all
      } yield fst ++ snd
    }
  }
}

trait Prelude {
  def all(implicit rec: RecursionEngine): OrCompilationError[Vector[CompiledDefinition]]
}

