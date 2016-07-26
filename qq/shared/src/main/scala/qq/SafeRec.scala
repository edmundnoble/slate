package qq

import matryoshka.{Corecursive, Recursive}
import matryoshka.Recursive.ops._

import scala.language.higherKinds
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import scalaz.{Monad, Trampoline, Traverse}
import Util._

import scalaz.Free.Trampoline

object SafeRec {

  @inline def recLimitStack[I, O]
  (i: I, recStep: (I, I => Trampoline[O]) => Trampoline[O], maxStackSize: Int): Trampoline[O] = {
    def loop(in: I, stackSize: Int = 0): Trampoline[O] =
      if (stackSize >= maxStackSize)
        Trampoline.suspend(recStep(in, loop(_)))
      else
        recStep(in, loop(_, stackSize + 1))

    loop(i)
  }

  def cata[T[_[_]], F[_] : Traverse, A](tf: T[F])(destroy: F[A] => A, maxStackSize: Int = 200)(implicit Re: Recursive[T]): Trampoline[A] = {
    recLimitStack[T[F], A](tf, (in, loop) => in.project.traverse[Trampoline, A](loop) map destroy, maxStackSize)
  }

  def cataM[T[_[_]], F[_] : Traverse, M[_] : Monad, A](tf: T[F])(destroy: F[A] => M[A], maxStackSize: Int = 200)(implicit Re: Recursive[T]): Trampoline[M[A]] = {
    recLimitStack[T[F], M[A]](tf, (in, loop) => in.project.traverse[Trampoline, M[A]](loop) map (_.sequence[M, A].flatMap(destroy)), maxStackSize)
  }

  def transCataT[T[_[_]], F[_] : Traverse](tf: T[F])(rewrite: T[F] => T[F], maxStackSize: Int = 200)
                                                    (implicit Re: Recursive[T], Co: Corecursive[T]): Trampoline[T[F]] = {
    recLimitStack[T[F], T[F]](tf, (in, loop) => in.project.traverse[Trampoline, T[F]](loop) map (ftf => rewrite(Co.embed[F](ftf))), maxStackSize)
  }

}

