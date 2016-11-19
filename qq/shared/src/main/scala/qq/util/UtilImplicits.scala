package qq
package util

import cats.{Applicative, Monad}
import cats.data.Validated
import cats.implicits._
import monix.eval.Task
import scodec._
import scodec.bits.BitVector
import shapeless.Lazy

import scala.language.{higherKinds, implicitConversions}

trait UtilImplicits {

  implicit def taskToParallelOpsConv[A](task: Task[A]): taskToParallelOps[A] = new taskToParallelOps(task)

  implicit def taskParallelOpsConv[A](task: TaskParallel[A]): taskParallelOps[A] = new taskParallelOps(task)

  implicit def validatedFlattenOpsConv[E, A](va: Validated[E, A]): validatedFlattenOps[E, A] = new validatedFlattenOps(va)

  // Monad with ap inconsistent with bind, for parallel operations on Tasks
  // (used for task-parallelism in QQ's compiler)
  implicit val TaskParMonad: Monad[TaskParallel] =
  new Monad[TaskParallel] {
    override def pure[A](a: A): TaskParallel[A] = Task.now(a).parallel

    override def flatMap[A, B](fa: TaskParallel[A])(f: (A) => TaskParallel[B]): TaskParallel[B] =
      fa.unwrap.flatMap(f(_).unwrap).parallel

    override def tailRecM[A, B](a: A)(f: (A) => TaskParallel[Either[A, B]]): TaskParallel[B] =
      Task.defer(f(a).unwrap).flatMap {
        case Right(b) =>
          Task.now(b)
        case Left(nextA) =>
          Task.suspend(tailRecM(nextA)(f).unwrap)
      }.parallel
  }

  implicit val TaskParAp: Applicative[TaskParallel] = new Applicative[TaskParallel] {
    override def pure[A](a: A): TaskParallel[A] = Task.now(a).parallel

    override def ap[A, B](ff: TaskParallel[(A) => B])(f: TaskParallel[A]): TaskParallel[B] =
      Task.mapBoth(ff.unwrap, f.unwrap)(_ (_)).parallel
  }

  // sum type encoded with a single bit
  implicit def eitherCodec[E, A](implicit E: Lazy[Codec[E]], A: Lazy[Codec[A]]): Codec[E Either A] = {
    val enc = Encoder.apply {
      (v: E Either A) =>
        v.fold(e => E.value.encode(e).map(BitVector.one ++ _), a => A.value.encode(a).map(BitVector.zero ++ _))
    }
    val dec = Decoder.apply {
      (in: BitVector) =>
        if (in.isEmpty) {
          Attempt.failure(Err.insufficientBits(1, 0))
        } else {
          if (in.head) {
            E.value.decode(in.tail).map(_.map(Either.left))
          } else {
            A.value.decode(in.tail).map(_.map(Either.right))
          }
        }
    }
    Codec(enc, dec)
  }

  // higher-kinded sum type encoded with a single bit
  implicit def coproductCodec[F[_], G[_], A](implicit E: Lazy[Codec[F[A]]], A: Lazy[Codec[G[A]]]): Codec[cats.data.Coproduct[F, G, A]] = {
    val enc = Encoder {
      (v: cats.data.Coproduct[F, G, A]) =>
        v.run.fold(e => E.value.encode(e).map(BitVector.one ++ _), a => A.value.encode(a).map(BitVector.zero ++ _))
    }
    val dec = Decoder.apply {
      (in: BitVector) =>
        if (in.isEmpty) {
          Attempt.failure(Err.insufficientBits(1, 0))
        } else {
          if (in.head) {
            E.value.decode(in.tail).map(_.map(x => cats.data.Coproduct[F, G, A](Either.left(x))))
          } else {
            A.value.decode(in.tail).map(_.map(x => cats.data.Coproduct[F, G, A](Either.right(x))))
          }
        }
    }
    Codec(enc, dec)
  }

}

final class taskToParallelOps[A](val task: Task[A]) extends AnyVal {
  @inline def parallel: TaskParallel[A] = shapeless.tag[Parallel][Task[A]](task)
}

final class taskParallelOps[A](val task: TaskParallel[A]) extends AnyVal {
  @inline def unwrap: Task[A] = task
}

final class validatedFlattenOps[E, A](val va: Validated[E, A]) extends AnyVal {
  def flatten[A1](implicit ev: Validated[E, A1] =:= A): Validated[E, A1] = va match {
    case f: cats.data.Validated.Invalid[E] => f
    case cats.data.Validated.Valid(v) => v.asInstanceOf[Validated[E, A1]]
  }
  def flatMap[B, EE <: E](f: A => Validated[EE, B]): Validated[E, B] = va match {
    case f: cats.data.Validated.Invalid[E] => f
    case cats.data.Validated.Valid(v) => f(v)
  }
}
