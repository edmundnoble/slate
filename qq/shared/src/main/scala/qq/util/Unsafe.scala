package qq
package util

import cats.implicits._
import cats.{Applicative, Eval, Traverse}

import scala.collection.mutable
import scala.language.higherKinds

object Unsafe {

  // Proof that forall A, F[A] is a subtype of G[A]
  trait Liskov1[F[_], G[_]] {
    def apply[A]: F[A] <:< G[A]
  }

  object Liskov1 {
    implicit object seq extends Liskov1[Seq, Iterable] {
      override def apply[A]: Seq[A] <:< Iterable[A] = implicitly
    }
  }

  trait GenericBuilderFactory[S[_]] {
    def newBuilder[A]: mutable.Builder[A, S[A]]
  }
  object GenericBuilderFactory {
    implicit object seq extends GenericBuilderFactory[Seq] {
      override def newBuilder[A]: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
    }
  }

  // Ever want to traverse anything with a CanBuildFrom which is Iterable?
  // I did too, at first
  final def builderTraverse[S[_]](implicit cbf: GenericBuilderFactory[S], ev: Liskov1[S, Iterable]): Traverse[S] = new Traverse[S] {
    override def traverse[G[_], A, B](fa: S[A])(f: (A) => G[B])(implicit evidence: Applicative[G]): G[S[B]] = {
      val bldr = cbf.newBuilder[B]
      bldr.sizeHint(ev.apply(fa))
      var acc: G[mutable.Builder[B, S[B]]] = evidence.pure(bldr)
      val iter = ev.apply(fa).iterator
      while (iter.hasNext) {
        val elem: A = iter.next()
        val comp: G[B] = f(elem)
        acc = (acc |@| comp).map { (a, c) =>
          a += c
          a
        }
      }
      acc.map(_.result())
    }
    override def foldLeft[A, B](fa: S[A], b: B)(f: (B, A) => B): B = ev.apply[A](fa).foldLeft(b)(f)
    // TODO
    override def foldRight[A, B](fa: S[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

  final def mapTraverse[K]: Traverse[({type F[A] = Map[K, A]})#F] = new Traverse[({type F[A] = Map[K, A]})#F] {
    override def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit evidence: Applicative[G]): G[Map[K, B]] = {
      val bldr = Map.newBuilder[K, B]
      bldr.sizeHint(fa)
      var acc = evidence.pure(bldr)
      val keys = fa.keysIterator
      while (keys.hasNext) {
        val key: K = keys.next()
        val elem: A = fa(key)
        val comp: G[B] = f(elem)
        acc = (acc, comp).map2 { (a, c) =>
          a += ((key, c))
          a
        }
      }
      acc.map(_.result())
    }
    // TODO
    override def foldLeft[A, B](fa: Map[K, A], b: B)(f: (B, A) => B): B = ???
    override def foldRight[A, B](fa: Map[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???
  }

}

