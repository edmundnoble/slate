package dash
package test

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" taggedAs WebTest in {
    container shouldBe displayed
    reactRoot shouldBe displayed
  }

  "title should be Slate" taggedAs WebTest in {
    pageTitle shouldBe "Slate"
  }

}
