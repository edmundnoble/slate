package dash.test

import monix.reactive.Observable
import org.scalatest.Succeeded

import scala.concurrent.duration._

class StartupTests extends UITestSuite {

  import Elements._

  "roots should be displayed" taggedAs WebTest in {
    container shouldBe displayed
    reactRoot shouldBe displayed
  }

  "appbar should be displayed with text" taggedAs WebTest in {
    appbar shouldBe displayed
    appbar.text shouldBe "Dashboarder"
  }

  "title should be Dashboarder" taggedAs WebTest in {
    pageTitle shouldBe "Dashboarder"
  }

}
