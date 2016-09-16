package qq
package util

import monix.eval.Task
import scodec._
import scodec.bits.BitVector
import shapeless.Lazy

import scala.language.{higherKinds, implicitConversions}
import scalaz.Tags.Parallel
import scalaz.syntax.either._
import scalaz.{Monad, \/}

trait UtilImplicits {

  // Monad with ap inconsistent with bind, for parallel operations on Tasks
  // (used for task-parallelism in QQ's compiler)
  implicit val TaskParMonad: Monad[TaskParallel] =
  new Monad[TaskParallel] {
    override def point[A](a: => A): TaskParallel[A] = Parallel(Task.now(a))

    override def ap[A, B](fa: => TaskParallel[A])(f: => TaskParallel[(A) => B]): TaskParallel[B] = {
      Parallel(Task.mapBoth(Parallel.unwrap(fa), Parallel.unwrap(f))((a, f) => f(a)))
    }

    override def bind[A, B](fa: TaskParallel[A])(f: (A) => TaskParallel[B]): TaskParallel[B] =
      Parallel(Parallel.unwrap(fa).flatMap(a => Parallel.unwrap(f(a))))
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

  // higher-kinded sum type encoded with a single bit
  implicit def coproductCodec[F[_], G[_], A](implicit E: Lazy[Codec[F[A]]], A: Lazy[Codec[G[A]]]): Codec[scalaz.Coproduct[F, G, A]] = {
    val enc = Encoder {
      (v: scalaz.Coproduct[F, G, A]) =>
        v.run.fold(e => E.value.encode(e).map(BitVector.one ++ _), a => A.value.encode(a).map(BitVector.zero ++ _))
    }
    val dec = Decoder.apply {
      (in: BitVector) =>
        if (in.isEmpty) {
          Attempt.failure(Err.insufficientBits(1, 0))
        } else {
          if (in.head) {
            E.value.decode(in.tail).map(_.map(x => scalaz.Coproduct[F, G, A](x.left[G[A]])))
          } else {
            A.value.decode(in.tail).map(_.map(x => scalaz.Coproduct[F, G, A](x.right[F[A]])))
          }
        }
    }
    Codec(enc, dec)
  }


}
