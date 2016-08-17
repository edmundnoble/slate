package qq

import qq.Recursion.RecursionEngine
import qq.Unsafe.{GenericBuilderFactory, Liskov1}

import scala.scalajs.js
import scalaz.Liskov
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
    }
  }
}
