package qq

import org.scalatest.OptionValues._
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

import scala.language.implicitConversions
import scalaz.\/

trait TestUtil {
  implicit def convertDisjunctionToValuable[E, A](dis: E \/ A)(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(dis.toOption, pos)

  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}

