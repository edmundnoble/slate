package dash.test

import java.io.File

import org.openqa.selenium.chrome.ChromeOptions
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import org.scalatest.selenium.Page
import org.scalatest.time.{Seconds, Span}

class StartupTests extends UITestSuite {

  "testy test" in {
    implicitlyWait(Span(4, Seconds))
    println(pageSource)
  }

}
