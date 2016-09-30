package qq
package util

import matryoshka.Recursive.ops._
import matryoshka.TraverseT.ops._
import matryoshka.{Corecursive, Fix, Recursive, TraverseT}

import scala.language.higherKinds
import scalaz._
import Scalaz._
import scalaz.Free.Trampoline

object Recursion {

  // recursion through continuation passing
  // TODO: make this a category somehow?
  trait RecursiveFunction[I, O] extends Any {

    def run(in: I, loop: I => Trampoline[O]): Trampoline[O]

    final def apply(in: I)(implicit engine: RecursionEngine): O = engine match {
      case Safe.Trampoline => Safe.recTrampoline(this, in)
      case Unsafe.Direct => Unsafe.recDirect(this, in)
      case Unsafe.LimitStack(limit) => Unsafe.recLimitStack(this, limit, in)
    }

  }

  sealed trait RecursionEngine extends Any

  object Safe {

    case object Trampoline extends RecursionEngine

    @inline final private[Recursion] def recTrampoline[I, O]
    (recStep: RecursiveFunction[I, O], i: I): O = {
      def loop(in: I): Trampoline[O] =
        scalaz.Trampoline.suspend(recStep.run(in, loop))

      loop(i).run
    }

  }

  object Unsafe {
    case class LimitStack(maxStackSize: Int) extends AnyVal with RecursionEngine
    case object Direct extends RecursionEngine

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
  (destroy: F[A] => A): RecursiveFunction[T[F], A] = new RecursiveFunction[T[F], A] {
    override def run(tf: T[F], loop: T[F] => Trampoline[A]): Trampoline[A] = {
      val projected = tf.project
      val looped = projected.traverse[Trampoline, A](loop)
      val destroyed = looped map destroy
      destroyed
    }
  }

  // if I find this useful I will feel very good about myself
  @inline final def cata2M[T[_[_]] : Recursive, F[_] : Traverse, M[_] : Monad, A]
  (destroy2: F[F[A]] => M[A]): RecursiveFunction[T[F], M[A]] = new RecursiveFunction[T[F], M[A]] {
    override def run(tf: T[F], loop: T[F] => Trampoline[M[A]]): Trampoline[M[A]] = {
      val project2: F[F[T[F]]] = tf.project.map(_.project)
      val looped: F[F[Trampoline[M[A]]]] = project2.map(_.map(loop))
      val traverseTrampoline: Trampoline[F[F[M[A]]]] = looped.traverse(_.sequence[Trampoline, M[A]])
      val traverseM: Trampoline[M[F[F[A]]]] = traverseTrampoline.map(_.traverse(_.sequence))
      traverseM.map(_.flatMap(destroy2))
    }
  }

  @inline final def cataM[T[_[_]] : Recursive, F[_] : Traverse, M[_] : Monad, A]
  (destroy: F[A] => M[A]): RecursiveFunction[T[F], M[A]] = new RecursiveFunction[T[F], M[A]] {
    override def run(tf: T[F], loop: T[F] => Trampoline[M[A]]): Trampoline[M[A]] = {
      val projected: F[T[F]] = tf.project
      val looped: Trampoline[F[M[A]]] = projected.traverse[Trampoline, M[A]](loop)
      val sequenced: Trampoline[M[F[A]]] = looped.map(_.sequence[M, A])
      val destroyed: Trampoline[M[A]] = sequenced.map(_.flatMap(destroy))
      destroyed
    }
  }

  @inline final def transAnaT[T[_[_]] : TraverseT, F[_] : Traverse]
  (rewrite: T[F] => T[F]): RecursiveFunction[T[F], T[F]] = new RecursiveFunction[T[F], T[F]] {
    override def run(tf: T[F], loop: T[F] => Trampoline[T[F]]): Trampoline[T[F]] =
      tf.traverse[Trampoline, F] { (ftf: F[T[F]]) =>
        val rewritten: F[T[F]] = ftf.map(rewrite)
        val looped: Trampoline[F[T[F]]] = rewritten.traverse(loop)
        looped
      }
  }

  @inline final def transCataT[T[_[_]] : TraverseT, F[_] : Traverse]
  (rewrite: T[F] => T[F]): RecursiveFunction[T[F], T[F]] = new RecursiveFunction[T[F], T[F]] {
    override def run(tf: T[F], loop: T[F] => Trampoline[T[F]]): Trampoline[T[F]] = {
      val looped: Trampoline[T[F]] = tf.traverse[Trampoline, F]((ftf: F[T[F]]) => ftf.traverse(loop))
      val rewritten: Trampoline[T[F]] = looped.map(rewrite)
      rewritten
    }
  }

  @inline final def allocate[T[_[_]] : Recursive, F[_] : Functor](tf: T[F]): Fix[F] =
    tf.cata(Fix[F])

  @inline final def stream[T[_[_]], F[_] : Functor](tf: Fix[F])(implicit T: Corecursive[T]): T[F] =
    T.ana(tf)(_.unFix)

}

