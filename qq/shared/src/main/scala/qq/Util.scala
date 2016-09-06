package qq

import monix.eval.Task
import scodec._
import scodec.bits.BitVector
import shapeless.Lazy

import scala.language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scalaz.Tags.Parallel
import scalaz.syntax.either._
import scalaz.{@@, Applicative, Monad, Tag, \/, ~>}

object Util {

  type TaskParallel[A] = Task[A] @@ Parallel

  // Monad with ap inconsistent with bind, for parallel operations on Tasks
  implicit val TaskParMonad = new Monad[TaskParallel] {
    override def point[A](a: => A) = Parallel(Task.now(a))
    override def ap[A, B](fa: => TaskParallel[A])(f: => TaskParallel[(A) => B]): TaskParallel[B] = {
      Parallel(Task.mapBoth(Parallel.unwrap(fa), Parallel.unwrap(f))((a, f) => f(a)))
    }
    override def bind[A, B](fa: TaskParallel[A])(f: (A) => TaskParallel[B]): TaskParallel[B] =
      Parallel(Parallel.unwrap(fa).flatMap(a => Parallel.unwrap(f(a))))
  }

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

  // sum type encoded with a single bit
  implicit def eitherCodec[E, A](implicit E: Lazy[Codec[E]], A: Lazy[Codec[A]]): Codec[E \/ A] = {
    val enc = Encoder.apply {
      (v: E \/ A) =>
        v.fold(e => E.value.encode(e).map(BitVector.one ++ _), a => A.value.encode(a).map(BitVector.zero ++ _))
    }
    val dec = Decoder.apply {
      (in: BitVector) =>
        if (in.isEmpty) {
          Attempt.failure(Err.insufficientBits(1, 0))
        } else {
          if (in.head) {
            E.value.decode(in.tail).map(_.map(_.left[A]))
          } else {
            A.value.decode(in.tail).map(_.map(_.right[E]))
          }
        }
    }
    Codec(enc, dec)
  }

}
