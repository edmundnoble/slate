package slate
package util

import scalajs.js

class DelimitTransformSuite extends SlateSuite {

  import DelimitTransform._
  import scala.scalajs.js.JSConverters._

  "simple" - {
    "string" in {
      string.toInterpret("hello").value shouldBe "hello"
      string.fromInterpret("hello") shouldBe "hello"
    }
    "arr" in {
      string.splitBy("d").toInterpret("hello").value.toArray shouldBe Array("hello")
      string.splitBy("d").toInterpret("heldlo").value.toArray shouldBe Array("hel", "lo")
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
