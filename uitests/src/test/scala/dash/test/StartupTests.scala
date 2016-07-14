package dash.test

class StartupTests extends UITestSuite {

  "roots should show up" in {
    find(Newtab.container).value should be('displayed)
    find(Newtab.reactRoot).value should be('displayed)
  }

}
