package qq

import org.scalatest.{Matchers, OptionValues, Tag}

trait QQTestSuite extends TestUtil with Matchers with OptionValues {
  object StackTest extends Tag("StackTest")
}
