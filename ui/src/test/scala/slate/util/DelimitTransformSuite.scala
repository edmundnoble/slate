package slate
package util

class DelimitTransformSuite extends SlateSuite {

  import DelimitTransform._

  "simple" - {
    "string" in {
      string.to("hello").value shouldBe "hello"
      string.from("hello") shouldBe "hello"
    }
    "arr" in {
      string.splitBy("d").to("hello").value.toList shouldBe List("hello")
      string.splitBy("d").to("heldlo").value.toList shouldBe List("hel", "lo")
    }
    "then" in {
      (string | "o" | string).to("helloworld").value shouldBe (("hell", "world"))
    }
  }
  "compound" - {
    "arr - join - arr" in {
      val (r1, r2) =
        (string.splitBy(",") | ":" | string.splitBy(";")).to("hello,world:bonjour;monde").value
      (r1.toList, r2.toList) shouldBe ((List("hello", "world"), List("bonjour", "monde")))
    }
  }
}
