package qq

import cats.data.Validated
import org.scalactic.Uniformity
import org.scalatest.OptionValues._
import qq.data.JSON
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

trait TestUtil {

  // this is necessary to encode the isomorphism between ObjList and ObjMap
  // into the equality checked by tests
  implicit object canonicalized extends Uniformity[JSON] {
    final def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[JSON]
    final def normalizedOrSame(b: Any): Any =
      b match {
        case s: JSON => normalized(s)
        case _ => b
      }
    def normalized(right: JSON): JSON = toCanonical(right)
  }

  def toCanonical(j: JSON): JSON = j match {
    case (l: JSON.ObjList) => JSON.ObjList(l.value.map { case (k, v) => k -> toCanonical(v) }.sortBy(_._1)(Ordering[String].reverse))
    case (m: JSON.ObjMap) => toCanonical(m.toList)
    case JSON.Arr(values) => JSON.Arr(values.map(toCanonical))
    case _ => j
  }

  implicit def convertEitherToValuable[E, A](either: E Either A)(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(either.right.toOption, pos)

  implicit def convertValidatedToValuable[E, A](dis: Validated[E, A])(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(dis.toOption, pos)

  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}

