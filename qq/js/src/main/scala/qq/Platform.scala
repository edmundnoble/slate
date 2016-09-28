package qq

import qq.util.Recursion
import qq.util.Recursion.RecursionEngine
import qq.util.Unsafe.{GenericBuilderFactory, Liskov1}

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.{UndefOr, WrappedArray}
import scalaz.Liskov.<~<
import scalaz.{Functor, Liskov}

object Platform {
  object Rec {
    implicit val defaultRecScheme: RecursionEngine =
       Recursion.Unsafe.LimitStack(maxStackSize = 50)
  }
  object Js {
    object Unsafe {
      implicit val jsWrappedArray: GenericBuilderFactory[js.WrappedArray] = new GenericBuilderFactory[js.WrappedArray] {
        override def newBuilder[A]: mutable.Builder[A, WrappedArray[A]] = js.WrappedArray.newBuilder[A]
      }
      implicit val jsWrappedArrayLiskovSeq: Liskov1[js.WrappedArray, Iterable] = new Liskov1[js.WrappedArray, Iterable] {
        override def apply[A]: js.WrappedArray[A] <~< Iterable[A] = Liskov.isa[js.WrappedArray[A], Iterable[A]]
      }
    }
  }
}
