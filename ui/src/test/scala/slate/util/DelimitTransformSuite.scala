package slate
package util

class DelimitTransformSuite extends SlateSuite {

  import DelimitTransform._
  import scala.scalajs.js.JSConverters._

  "simple" - {
    "string" in {
      string.toInterpret("hello").value shouldBe "hello"
      string.fromInterpret("hello") shouldBe "hello"
    }
    "arr" in {
      string.splitBy("d").toInterpret("hello").value.toList shouldBe List("hello")
      string.splitBy("d").toInterpret("heldlo").value.toList shouldBe List("hel", "lo")
    }
    "then" in {
      (string | "o" | string).toInterpret("helloworld").value shouldBe (("hell", "world"))
    }
  }
  "compound" - {
    "arr - join - arr" in {
      val (r1, r2) =
        (string.splitBy(",") | ":" | string.splitBy(";")).toInterpret("hello,world:bonjour;monde").value
      (r1.toList, r2.toList) shouldBe ((List("hello", "world"), List("bonjour", "monde")))
    }
  }
}
