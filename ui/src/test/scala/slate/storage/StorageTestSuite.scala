package slate.storage

import org.scalatest.{FreeSpec, Matchers, OptionValues}
import qq.util.Recursion
import qq.util.Recursion.RecursionEngine

abstract class StorageTestSuite extends FreeSpec with Matchers with OptionValues {
  implicit val recEngine: RecursionEngine =
    Recursion.Unsafe.Direct
}
