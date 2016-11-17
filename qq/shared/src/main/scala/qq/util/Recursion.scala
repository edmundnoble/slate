package qq
package util

import cats.implicits._
import cats.{Eval, Monad, Traverse}

import scala.language.higherKinds

object Recursion {

  // recursion through continuation passing
  // TODO: make this a category somehow?
  trait RecursiveFunction[I, O] extends Any {

    def run(in: I, loop: I => Eval[O]): Eval[O]

    final def apply(in: I)(implicit engine: RecursionEngine): O = engine match {
      case Safe.Trampoline => Safe.recTrampoline(this, in)
      case Unsafe.Direct => Unsafe.recDirect(this, in)
      case Unsafe.LimitStack(limit) => Unsafe.recLimitStack(this, limit, in)
    }

  }

  sealed trait RecursionEngine extends Any

  object Safe {

    case object Trampoline extends RecursionEngine

    @inline final def recTrampoline[I, O]
    (recStep: RecursiveFunction[I, O], i: I): O = {
      def loop(in: I): Eval[O] =
        Eval.defer(recStep.run(in, loop))

      loop(i).value
    }

  }

  object Unsafe {
    case class LimitStack(maxStackSize: Int) extends AnyVal with RecursionEngine
    case object Direct extends RecursionEngine

    @inline final private[Recursion] def recDirect[I, O]
    (recStep: RecursiveFunction[I, O], i: I): O = {
      def loop(in: I): Eval[O] =
        recStep.run(in, loop)

      loop(i).value
    }

    @inline final private[Recursion] def recLimitStack[I, O]
    (recStep: RecursiveFunction[I, O], maxStackSize: Int, i: I): O = {
      def loop(in: I, stackSize: Int = 0): Eval[O] =
        if (stackSize >= maxStackSize)
          Eval.defer(recStep.run(in, loop(_)))
        else
          recStep.run(in, loop(_, stackSize + 1))

      loop(i).value
    }

  }

  @inline final def cata[F[_] : Traverse, A]
  (destroy: F[A] => A): RecursiveFunction[Fix[F], A] = new RecursiveFunction[Fix[F], A] {
    override def run(tf: Fix[F], loop: Fix[F] => Eval[A]): Eval[A] = {
      val projected = tf.unFix
      val looped = projected.traverse[Eval, A](loop)
      val destroyed = looped map destroy
      destroyed
    }
  }

  // if I find this useful I will feel very good about myself
  @inline final def cata2M[F[_] : Traverse, M[_] : Monad, A]
  (destroy2: F[F[A]] => M[A]): RecursiveFunction[Fix[F], M[A]] = new RecursiveFunction[Fix[F], M[A]] {
    override def run(tf: Fix[F], loop: Fix[F] => Eval[M[A]]): Eval[M[A]] = {
      val project2: F[F[Fix[F]]] = tf.unFix.map(_.unFix)
      val looped: F[F[Eval[M[A]]]] = project2.map(_.map(loop))
      val traverseEval: Eval[F[F[M[A]]]] = looped.traverse(_.sequence[Eval, M[A]])
      val traverseM: Eval[M[F[F[A]]]] = traverseEval.map(_.traverse(_.sequence))
      traverseM.map(_.flatMap(destroy2))
    }
  }

  @inline final def cataM[F[_] : Traverse, M[_] : Monad, A]
  (destroy: F[A] => M[A]): RecursiveFunction[Fix[F], M[A]] = new RecursiveFunction[Fix[F], M[A]] {
    override def run(tf: Fix[F], loop: Fix[F] => Eval[M[A]]): Eval[M[A]] = {
      val projected: F[Fix[F]] = tf.unFix
      val looped: Eval[F[M[A]]] = projected.traverse[Eval, M[A]](loop)
      val sequenced: Eval[M[F[A]]] = looped.map(_.sequence[M, A])
      val destroyed: Eval[M[A]] = sequenced.map(_.flatMap(destroy))
      destroyed
    }
  }

  @inline final def transAnaT[F[_] : Traverse]
  (rewrite: Fix[F] => Fix[F]): RecursiveFunction[Fix[F], Fix[F]] = new RecursiveFunction[Fix[F], Fix[F]] {
    override def run(tf: Fix[F], loop: Fix[F] => Eval[Fix[F]]): Eval[Fix[F]] = {
      val ftf: F[Fix[F]] = tf.unFix
      val rewritten: F[Fix[F]] = ftf.map(rewrite)
      val looped: Eval[F[Fix[F]]] = rewritten.traverse(loop)
      looped.map(Fix[F])
    }
  }

  @inline final def transCataT[F[_] : Traverse]
  (rewrite: Fix[F] => Fix[F]): RecursiveFunction[Fix[F], Fix[F]] = new RecursiveFunction[Fix[F], Fix[F]] {
    override def run(tf: Fix[F], loop: Fix[F] => Eval[Fix[F]]): Eval[Fix[F]] = {
      val looped: Eval[Fix[F]] = tf.unFix.traverse(loop).map(Fix[F])
      val rewritten: Eval[Fix[F]] = looped.map(rewrite)
      rewritten
    }
  }

}

