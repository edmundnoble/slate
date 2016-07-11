package dash.test

import java.io.File

import org.openqa.selenium.chrome.ChromeDriverService

class WebDriverFramework extends utest.runner.Framework {
  override def setup() = {
    WebDriver.chromeDriverService.start()
  }
  override def teardown() = {
    WebDriver.chromeDriverService.stop()
  }
}

object WebDriver {
  val chromeDriverService =
    new ChromeDriverService.Builder()
      .usingDriverExecutable(new File("/usr/local/sbin/chromedriver"))
      .usingAnyFreePort()
      .build()
}
