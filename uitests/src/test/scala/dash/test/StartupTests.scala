package dash.test

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" in {
    container shouldBe displayed
    reactRoot shouldBe displayed
  }

  "appbar should be displayed with text" in {
    appbar shouldBe displayed
    appbar.text shouldBe "Dashboarder"
  }

}
