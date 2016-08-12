package dash.test

import java.io.File

import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.selenium.WebBrowser
import org.scalatest.time.{Seconds, Span}

abstract class UITestSuite extends FreeSpec with Matchers with WebBrowser with BeforeAndAfterAll with OptionValues {
  lazy val displayed = new BeMatcher[Element] {
    override def apply(left: Element): MatchResult =
      MatchResult(
        left.isDisplayed,
        left.toString + " was not displayed",
        left.toString + " was displayed"
      )
  }

  @volatile var chromeDriverServiceInitialized = false
  lazy val chromeDriverService: ChromeDriverService = {

    val service = new ChromeDriverService.Builder()
      .usingDriverExecutable(new File("/usr/local/sbin/chromedriver"))
      .usingAnyFreePort()
      .build()

    service.start()
    chromeDriverServiceInitialized = true
    service
  }

  lazy val options = {
    val extensionPath = new File("ui/target/chrome/unpackedfast").getAbsolutePath
    val opts = new ChromeOptions()
    opts.addArguments(s"load-extension=$extensionPath")
    opts
  }

  lazy val capabilities = {
    val caps = new DesiredCapabilities()
    caps.setCapability(ChromeOptions.CAPABILITY, options)
    caps
  }

  object WebTestTag extends Tag("WebTestTag")

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

  override protected def afterAll(): Unit = {
    super.afterAll()
    if (chromeDriverServiceInitialized) {
      chromeDriverService.stop()
    }
  }
}

