package qq

import qq.data.JSON
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

trait TestUtil {

  // this is necessary to encode the isomorphism between ObjList and ObjMap
  // into the equality checked by tests
  implicit final class normJSON(json: JSON) {
    def norm: JSON = toCanonical(json)
  }

  def toCanonical(j: JSON): JSON = j match {
    case (l: JSON.ObjList) => JSON.ObjList(l.value.map { case (k, v) => k -> toCanonical(v) }.sortBy(_._1)(Ordering[String].reverse))
    case (m: JSON.ObjMap) => toCanonical(m.toList)
    case JSON.Arr(values) => JSON.Arr(values.map(toCanonical))
    case _ => j
  }

  implicit final class eitherValueOps[E, A](either: E Either A) {
    def rightValue: A = {
      assert(either.isRight, s"rightValue called on $either")
      either.right.get
    }
    def leftValue: E = {
      assert(either.isLeft, s"rightValue called on $either")
      either.left.get
    }
  }

  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}

