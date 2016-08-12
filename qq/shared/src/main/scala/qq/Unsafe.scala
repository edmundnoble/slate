package qq

import scalaz.syntax.applicative._
import scalaz.{Applicative, Traverse}

object Unsafe {

  // these instances are unsafe because they are for mutable AND immutable collections

  implicit final def seqTraverse[T]: Traverse[Seq] = new Traverse[Seq] {
    override def traverseImpl[G[_], A, B](fa: Seq[A])(f: (A) => G[B])(implicit evidence: Applicative[G]): G[Seq[B]] = {
      val bldr = Seq.newBuilder[B]
      bldr.sizeHint(fa)
      var acc = evidence.point(bldr)
      val iter = fa.iterator
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

  implicit final def mapTraverse[K]: Traverse[({type F[A] = Map[K, A]})#F] = new Traverse[({type F[A] = Map[K, A]})#F] {
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

