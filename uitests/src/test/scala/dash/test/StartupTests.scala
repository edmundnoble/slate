package dash.test

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" taggedAs WebTestTag in {
    container shouldBe displayed
    reactRoot shouldBe displayed
  }

  "appbar should be displayed with text" taggedAs WebTestTag in {
    appbar shouldBe displayed
    appbar.text shouldBe "Dashboarder"
  }

}
