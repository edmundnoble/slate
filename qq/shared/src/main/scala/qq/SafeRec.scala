package qq

import matryoshka.{Corecursive, Recursive}
import matryoshka.Corecursive.ops._
import matryoshka.Recursive.ops._

import scala.language.higherKinds
import scala.util.control.TailCalls.TailRec
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import scalaz.{Functor, Monad, Traverse}
import Util._

import scala.util.control.TailCalls

object SafeRec {

  def cata[T[_[_]], F[_] : Traverse, A](tf: T[F])(destroy: F[A] => A)(implicit Re: Recursive[T]): TailRec[A] = {
    TailCalls.tailcall(tf.project traverse (cata(_)(destroy)) map destroy)
  }

  /** A Kleisli catamorphism. */
  def cataM[T[_[_]], F[_] : Traverse, M[_] : Monad, A](tf: T[F])(destroy: F[A] => M[A])(implicit Re: Recursive[T]): TailRec[M[A]] = {
    tf.project traverse (cataM(_)(destroy)) map (_.sequence[M, A].flatMap(destroy))
  }

  def transCataT[T[_[_]], F[_] : Traverse](tf: T[F])(rewrite: T[F] => T[F])(implicit Re: Recursive[T], Co: Corecursive[T]): TailRec[T[F]] = {
    TailCalls.tailcall(tf.project traverse (transCataT(_)(rewrite)) map (ftf => rewrite(Co.embed[F](ftf))))
  }

}

