package qq

import scalaz.\/

import org.scalatest.OptionValues._

trait TestUtil {
  implicit def convertDisjunctionToValuable[E, A](dis: E \/ A)(implicit pos: org.scalactic.source.Position): Valuable[A] =
    new Valuable(dis.toOption, pos)
}

object TestUtil extends TestUtil
