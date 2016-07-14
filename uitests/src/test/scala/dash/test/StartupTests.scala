package dash.test

class StartupTests extends UITestSuite {

  "roots should be displayed" in {
    Elements.container should be(displayed)
    Elements.reactRoot should be(displayed)
  }

  "appbar should be displayed with text" in {
    Elements.appbar should be(displayed)
    Elements.appbar.text should be("Dashboarder")
  }

}
