package qq

import scala.collection.SeqLike
import scala.collection.generic.{CanBuildFrom, GenericCompanion}
import scala.collection.mutable
import scalaz.Liskov.<~<
import scalaz.syntax.applicative._
import scalaz.{Applicative, Liskov, Traverse}

object Unsafe {

  trait Liskov1[F[_], G[_]] {
    def apply[A]: F[A] <~< G[A]
  }

  object Liskov1 {
    implicit val seq: Liskov1[Seq, Iterable] = new Liskov1[Seq, Iterable] {
      override def apply[A] = Liskov.isa[Seq[A], Iterable[A]]
    }
  }

  trait GenericBuilderFactory[S[_]] {
    def newBuilder[A]: mutable.Builder[A, S[A]]
  }
  object GenericBuilderFactory {
    implicit val seq: GenericBuilderFactory[Seq] = new GenericBuilderFactory[Seq] {
      override def newBuilder[A] = Seq.newBuilder[A]
    }
  }

  final def builderTraverse[S[_]](implicit cbf: GenericBuilderFactory[S], ev: Liskov1[S, Iterable]): Traverse[S] = new Traverse[S] {
    override def traverseImpl[G[_], A, B](fa: S[A])(f: (A) => G[B])(implicit evidence: Applicative[G]): G[S[B]] = {
      val bldr = cbf.newBuilder[B]
      bldr.sizeHint(ev.apply(fa))
      var acc = evidence.point(bldr)
      val iter = ev.apply(fa).iterator
      while (iter.hasNext) {
        val elem: A = iter.next()
        val comp: G[B] = f(elem)
        acc = (acc |@| comp) { (a, c) =>
          a += c
          a
        }
      }
      acc.map(_.result())
    }
  }

  final def mapTraverse[K]: Traverse[({type F[A] = Map[K, A]})#F] = new Traverse[({type F[A] = Map[K, A]})#F] {
    override def traverseImpl[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit evidence: Applicative[G]): G[Map[K, B]] = {
      val bldr = Map.newBuilder[K, B]
      bldr.sizeHint(fa)
      var acc = evidence.point(bldr)
      val keys = fa.keysIterator
      while (keys.hasNext) {
        val key: K = keys.next()
        val elem: A = fa(key)
        val comp: G[B] = f(elem)
        acc = (acc |@| comp) { (a, c) =>
          a += ((key, c))
          a
        }
      }
      acc.map(_.result())
    }
  }

}

