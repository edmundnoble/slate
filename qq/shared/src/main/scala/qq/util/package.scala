package qq

import cats.~>
import monix.eval.Task
import shapeless.tag._

package object util extends UtilImplicits {
  type TaskParallel[A] = Task[A] @@ Parallel

  final def withPrefixes[A](xss: List[List[A]], ys: List[A]): List[List[A]] =
    for {xs <- xss; y <- ys} yield y :: xs

  final def foldWithPrefixes[A](firstPrefix: List[A], nextPrefixes: List[A]*): List[List[A]] =
    nextPrefixes.foldLeft(firstPrefix :: Nil)(withPrefixes)

  final def withPrefixesV[A](xss: Vector[Vector[A]], ys: Vector[A]): Vector[Vector[A]] =
    for {xs <- xss; y <- ys} yield y +: xs

  final def foldWithPrefixesV[A](firstPrefix: Vector[A], nextPrefixes: Vector[Vector[A]]): Vector[Vector[A]] =
    nextPrefixes.foldLeft(firstPrefix +: Vector.empty)(withPrefixesV)

  def single: Option ~> Seq = new (Option ~> Seq) {
    def apply[A](op: Option[A]): Seq[A] =
      op match {
        case None => Vector.empty[A]
        case Some(v) => Vector.empty[A] :+ v
      }
  }

  final def unionWithKey[K, A](m1: Map[K, A], m2: Map[K, A])(f: (K, A, A) => A): Map[K, A] = {
    val diff = m2 -- m1.keySet
    val aug = m1 map {
      case (k, v) => if (m2 contains k) k -> f(k, v, m2(k)) else (k, v)
    }
    aug ++ diff
  }

  final def unionWith[K, A](m1: Map[K, A], m2: Map[K, A])(f: (A, A) => A): Map[K, A] =
    unionWithKey(m1, m2)((_, x, y) => f(x, y))

  final def toMapWith[K, V](f: V => K)(seq: Seq[V]): Map[K, V] =
    seq.map(v => f(v) -> v)(collection.breakOut)

}
