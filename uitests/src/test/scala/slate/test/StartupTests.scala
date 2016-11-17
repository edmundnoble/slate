package slate
package test

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" taggedAs WebTest in {
    container.isDisplayed shouldBe true
    reactRoot.isDisplayed shouldBe true
  }

  "title should be Slate" taggedAs WebTest in {
    pageTitle shouldBe "Slate"
  }

}
