package slate
package util

import scalajs.js

class DelimitTransformSuite extends SlateSuite {

  import DelimitTransform._
  import scala.scalajs.js.JSConverters._

  "simple" - {
    "id" in {
      id.toInterpret("hello").value shouldBe "hello"
      id.fromInterpret("hello") shouldBe "hello"
    }
    "arr" in {
      id.splitBy("d").toInterpret("hello").value.toArray shouldBe Array("hello")
      id.splitBy("d").toInterpret("heldlo").value.toArray shouldBe Array("hel", "lo")
    }
    "then" in {
      (id | "o" | id).toInterpret("helloworld").value shouldBe (("hell", "world"))
    }
  }
  "compound" - {
    "arr - join - arr" in {
      val (r1, r2) =
        (id.splitBy(",") | ":" | id.splitBy(";")).toInterpret("hello,world:bonjour;monde").value
      (r1.toList, r2.toList) shouldBe ((List("hello", "world"), List("bonjour", "monde")))
    }
  }
}
