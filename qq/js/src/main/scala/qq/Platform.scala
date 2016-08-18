package qq

import qq.Recursion.RecursionEngine
import qq.Unsafe.{GenericBuilderFactory, Liskov1}

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scalaz.{Applicative, Functor, Liskov, Traverse}
import scalaz.Liskov.<~<

object Platform {
  object Rec {
    implicit val defaultRecScheme: RecursionEngine =
      Recursion.Unsafe.LimitStack(maxStackSize = 50)
  }
  object Js {
    object Unsafe {
      implicit val jsWrappedArray: GenericBuilderFactory[js.WrappedArray] = new GenericBuilderFactory[js.WrappedArray] {
        override def newBuilder[A] = js.WrappedArray.newBuilder[A]
      }
      implicit val jsWrappedArrayLiskovSeq: Liskov1[js.WrappedArray, Iterable] = new Liskov1[js.WrappedArray, Iterable] {
        override def apply[A]: js.WrappedArray[A] <~< Iterable[A] = Liskov.isa[js.WrappedArray[A], Iterable[A]]
      }
      final val UndefOrTraverse = new Functor[js.UndefOr] {
        override def map[A, B](fa: UndefOr[A])(f: (A) => B) = fa.map(f)
      }
    }
  }
}
