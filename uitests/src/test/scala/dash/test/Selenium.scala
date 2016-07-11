package dash.test

import java.io.File

import utest._
import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import utest.framework.TestPath

import scala.concurrent.{ExecutionContext, Future}

object Selenium extends utest.TestSuite {

  import WebDriver._

  val options = new ChromeOptions()
  val extensionPath = new File("ui/target/chrome/unpackedfast").getAbsolutePath
  options.addArguments(s"load-extension=$extensionPath")
  val capabilities = new DesiredCapabilities()
  capabilities.setCapability(ChromeOptions.CAPABILITY, options)

  override def tests = TestSuite {
    val chromeDriver = new RemoteWebDriver(chromeDriverService.getUrl, capabilities)
    chromeDriver.get("chrome://newtab")
    // insert tests here
    chromeDriver.close()
  }


}
