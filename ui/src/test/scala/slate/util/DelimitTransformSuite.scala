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
      string.splitBy("d").to("hello").value.toVector shouldBe Vector("hello")
      string.splitBy("d").to("heldlo").value.toVector shouldBe Vector("hel", "lo")
    }
    "then" in {
      (string | "o" | string).to("helloworld").value shouldBe (("hell", "world"))
    }
  }
  "compound" - {
    "arr - join - arr" in {
      val (r1, r2) =
        (string.splitBy(",") | ":" | string.splitBy(";")).to("hello,world:bonjour;monde").value
      (r1.toVector, r2.toVector) shouldBe ((Vector("hello", "world"), Vector("bonjour", "monde")))
    }
  }
}
