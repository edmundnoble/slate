package qq
package jsc

import scala.collection.GenMap
import scala.scalajs.js
import scala.scalajs.js.Dictionary
import scalaz.syntax.apply._
import scalaz.{Applicative, Traverse}

object JsUtil {
  implicit class JSRichSeq[T](val map: Seq[(String, T)]) extends AnyVal {
    @inline final def toJSDictionary: Dictionary[T] = {
      val result = Dictionary.empty[T]
      map.foreach { case (key, value) => result(key) = value }
      result
    }
  }

  implicit def jsArrayTraverse: Traverse[js.Array] = new Traverse[js.Array] {
    override def traverseImpl[G[_], A, B](fa: js.Array[A])(f: (A) => G[B])(implicit evidence: Applicative[G]): G[js.Array[B]] = {
      var acc: G[js.Array[B]] = evidence.point(new js.Array[B](fa.length))
      var i: Int = 0
      while (i < fa.length) {
        val elem: A = fa(i)
        val comp: G[B] = f(elem)
        acc = (acc |@| comp) { (a, c) =>
          a(i) = c
          a
        }
        i += 1
      }
      acc
    }
  }

}
