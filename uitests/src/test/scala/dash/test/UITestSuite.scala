package dash.test

import java.io.File

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import org.scalatest._
import org.scalatest.selenium.{Page, WebBrowser}

class UITestSuite extends FreeSpec with Matchers with WebBrowser with BeforeAndAfterAll {
  val chromeDriverService =
    new ChromeDriverService.Builder()
      .usingDriverExecutable(new File("/usr/local/sbin/chromedriver"))
      .usingAnyFreePort()
      .build()

  val options = new ChromeOptions()
  val extensionPath = new File("ui/target/chrome/unpackedfast").getAbsolutePath
  options.addArguments(s"load-extension=$extensionPath")
  val capabilities = new DesiredCapabilities()
  capabilities.setCapability(ChromeOptions.CAPABILITY, options)

  object Newtab extends Page {
    override val url: String = "chrome://newtab"
  }

  def makeChromeDriver(): RemoteWebDriver =
    new RemoteWebDriver(chromeDriverService.getUrl, capabilities)

  implicit var webDriver: WebDriver = _

  override def withFixture(test: NoArgTest): Outcome = {
    webDriver = makeChromeDriver()
    go to Newtab
    val outcome = super.withFixture(test)
    close()
    outcome
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    chromeDriverService.start()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    chromeDriverService.stop()
  }
}

