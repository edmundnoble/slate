package qq

import scalaz.\/
import org.scalatest.OptionValues._
import qq.Recursion.RecursionEngine

import scala.language.implicitConversions

trait TestUtil {
  implicit def convertDisjunctionToValuable[E, A](dis: E \/ A)(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(dis.toOption, pos)

  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}

