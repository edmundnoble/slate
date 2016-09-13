package qq

import monix.eval.Task

import scalaz.Tags.Parallel
import scalaz.{@@, ~>}

package object util extends UtilImplicits {
  type TaskParallel[A] = Task[A] @@ Parallel

  def withPrefixes[A](xss: List[List[A]], ys: List[A]): List[List[A]] =
    for {xs <- xss; y <- ys} yield y :: xs

  def foldWithPrefixes[A](firstPrefix: List[A], nextPrefixes: List[A]*): List[List[A]] =
    nextPrefixes.foldLeft(firstPrefix :: Nil)(withPrefixes)

  def single: Option ~> Seq = new (Option ~> Seq) {
    def apply[A](op: Option[A]): Seq[A] =
      op match {
        case None => Vector.empty[A]
        case Some(v) => Vector.empty[A] :+ v
      }
  }

  @inline def toMapWith[K, V](f: V => K)(seq: Seq[V]): Map[K, V] =
    seq.map(d => f(d) -> d)(collection.breakOut)

}
