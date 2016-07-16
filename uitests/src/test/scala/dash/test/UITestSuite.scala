package dash.test

import java.io.File

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.selenium.{Page, WebBrowser}
import org.scalatest.time.{Seconds, Span}

abstract class UITestSuite extends FreeSpec with Matchers with WebBrowser with BeforeAndAfterAll with OptionValues {
  val displayed = new BeMatcher[Element] {
    override def apply(left: Element): MatchResult =
      MatchResult(
        left.isDisplayed,
        left.toString + " was not displayed",
        left.toString + " was displayed"
      )
  }

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

  object Elements {
    def reactRoot = find(IdQuery("react-root")).value
    def container = find(IdQuery("container")).value
    def appbar = find(ClassNameQuery("mui-appbar")).value
  }

  def makeChromeDriver(): RemoteWebDriver =
    new RemoteWebDriver(chromeDriverService.getUrl, capabilities)

  implicit var webDriver: WebDriver = _

  override def withFixture(test: NoArgTest): Outcome = {
    webDriver = makeChromeDriver()
    implicitlyWait(Span(10, Seconds))
    go to "chrome://newtab"
    val outcome = super.withFixture(test)
    close()
    webDriver = null
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

