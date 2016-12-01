package slate
package app

import cats.{Applicative, Eval, Traverse}
import cats.implicits._

final case class SlateProgram[+P](id: Int, title: String, titleLink: String, program: P) {
  def withProgram[B](newProgram: B): SlateProgram[B] = copy(program = newProgram)
  def withoutProgram: SlateProgram[Unit] = copy(program = ())
}

object SlateProgram {

  implicit def slateProgramTraverse: Traverse[SlateProgram] = new Traverse[SlateProgram] {
    override def map[A, B](fa: SlateProgram[A])(f: (A) => B): SlateProgram[B] =
      fa.withProgram(f(fa.program))
    override def traverse[G[_], A, B](fa: SlateProgram[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[SlateProgram[B]] =
      f(fa.program).map(fa.withProgram)
    override def foldLeft[A, B](fa: SlateProgram[A], b: B)(f: (B, A) => B): B =
      f(b, fa.program)
    override def foldRight[A, B](fa: SlateProgram[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      f(fa.program, lb)
  }
}

