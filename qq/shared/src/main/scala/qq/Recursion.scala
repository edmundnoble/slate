package qq

import matryoshka.Recursive.ops._
import matryoshka.{Recursive, TraverseT}
import TraverseT.ops._
import scala.language.higherKinds
import scalaz.Free.Trampoline
import scalaz.syntax.monad._
import scalaz.syntax.traverse._
import scalaz.{Monad, Trampoline, Traverse}

object Recursion {

  // recursion through continuation passing
  // TODO: make this a category somehow?
  case class RecursiveFunction[I, O](run: (I, (I => Trampoline[O])) => Trampoline[O]) extends AnyVal {

    @inline final def apply(engine: RecursionEngine, in: I) = engine match {
      case Safe.RecursionTrampoline => Safe.recTrampoline(this, in)
      case Unsafe.RecursionDirect => Unsafe.recDirect(this, in)
      case Unsafe.RecursionLimitStack(limit) => Unsafe.recLimitStack(this, limit, in)
    }

  }

  sealed trait RecursionEngine extends Any

  object Safe {

    case object RecursionTrampoline extends RecursionEngine

    @inline final private[Recursion] def recTrampoline[I, O]
    (recStep: RecursiveFunction[I, O], i: I): O = {
      def loop(in: I): Trampoline[O] =
        Trampoline.suspend(recStep.run(in, loop))

      loop(i).run
    }

  }

  object Unsafe {
    case class RecursionLimitStack(maxStackSize: Int) extends AnyVal with RecursionEngine
    case object RecursionDirect extends RecursionEngine

    @inline final private[Recursion] def recDirect[I, O]
    (recStep: RecursiveFunction[I, O], i: I): O = {
      def loop(in: I): Trampoline[O] =
        recStep.run(in, loop)

      loop(i).run
    }

    @inline final private[Recursion] def recLimitStack[I, O]
    (recStep: RecursiveFunction[I, O], maxStackSize: Int, i: I): O = {
      def loop(in: I, stackSize: Int = 0): Trampoline[O] =
        if (stackSize >= maxStackSize)
          Trampoline.suspend(recStep.run(in, loop(_)))
        else
          recStep.run(in, loop(_, stackSize + 1))

      loop(i).run
    }

  }

  @inline final def cata[T[_[_]] : Recursive, F[_] : Traverse, A]
  (destroy: F[A] => A): RecursiveFunction[T[F], A] =
    RecursiveFunction { (tf, loop) =>
      tf.project.traverse[Trampoline, A](loop) map destroy
    }

  @inline final def cataM[T[_[_]] : Recursive, F[_] : Traverse, M[_] : Monad, A]
  (destroy: F[A] => M[A]): RecursiveFunction[T[F], M[A]] =
    RecursiveFunction { (tf, loop) =>
      tf.project.traverse[Trampoline, M[A]](loop) map (_.sequence[M, A].flatMap(destroy))
    }

  @inline final def transCataT[T[_[_]], F[_]]
  (rewrite: T[F] => T[F])(implicit F: Traverse[F], T: TraverseT[T]): RecursiveFunction[T[F], T[F]] =
    RecursiveFunction { (tf, loop) =>
      tf.traverse[Trampoline, F](ftf => ftf.traverse[Trampoline, T[F]](tf => loop(tf))).map(rewrite)
    }

}

