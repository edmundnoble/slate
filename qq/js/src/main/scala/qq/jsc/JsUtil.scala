package qq.jsc

import scala.scalajs.js
import scalaz.{Applicative, Traverse}
import scalaz.syntax.apply._

object JsUtil {

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
