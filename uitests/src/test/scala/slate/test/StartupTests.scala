package slate
package test

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" taggedAs WebTest ignore {
    container.isDisplayed shouldBe true
    reactRoot.isDisplayed shouldBe true
  }

  "title should be Slate" taggedAs WebTest ignore {
    pageTitle shouldBe "Slate"
  }

}
