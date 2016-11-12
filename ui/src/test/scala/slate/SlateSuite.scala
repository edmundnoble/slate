package slate

import org.scalatest.{FreeSpec, Matchers, OptionValues}
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

abstract class SlateSuite extends FreeSpec with Matchers with OptionValues {
  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}
