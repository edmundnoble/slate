package slate
package test

import java.io.File
import java.util.concurrent.TimeUnit

import monix.execution.schedulers.ExecutionModel
import monix.execution.{Cancelable, Scheduler}
import org.openqa.selenium.WebDriver
import org.openqa.selenium.chrome.{ChromeDriverService, ChromeOptions}
import org.openqa.selenium.remote.{DesiredCapabilities, RemoteWebDriver}
import org.scalatest._
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.selenium.WebBrowser
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.ExecutionContext

abstract class UITestSuite extends AsyncFreeSpec with Matchers with WebBrowser with BeforeAndAfterAll with OptionValues {
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
    val extensionPath = new File("ui/target/chrome/unpackedopt").getAbsolutePath
    val opts = new ChromeOptions()
    opts.addArguments("load-extension=" + extensionPath)
    opts
  }

  lazy val capabilities = {
    val caps = new DesiredCapabilities()
    caps.setCapability(ChromeOptions.CAPABILITY, options)
    caps
  }

  object WebTest extends Tag("WebTest")

  object Elements {
    def reactRoot = find(IdQuery("react-root")).value
    def container = find(IdQuery("container")).value
  }

  def makeChromeDriver(): RemoteWebDriver =
    new RemoteWebDriver(chromeDriverService.getUrl, capabilities)

  implicit var webDriver: WebDriver = _

  override def withFixture(test: NoArgAsyncTest): FutureOutcome = {
    webDriver = makeChromeDriver()
    implicitlyWait(Span(10, Seconds))
    go to "chrome://newtab"
    val outcome = super.withFixture(test)
    outcome.onSucceededThen {
      close()
      webDriver = null
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    if (chromeDriverServiceInitialized) {
      chromeDriverService.stop()
    }
  }

  implicit val schedulerVal: Scheduler = scheduler(super.executionContext)

  def scheduler(implicit executionContext: ExecutionContext): Scheduler =
    new Scheduler {
      override def execute(runnable: Runnable): Unit = executionContext.execute(runnable)
      override def reportFailure(t: Throwable): Unit = executionContext.reportFailure(t)
      override def scheduleOnce(initialDelay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        executionContext.execute(r)
        Cancelable.empty
      }
      override def scheduleWithFixedDelay(initialDelay: Long, delay: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        ???
      }
      override def scheduleAtFixedRate(initialDelay: Long, period: Long, unit: TimeUnit, r: Runnable): Cancelable = {
        ???
      }
      override def currentTimeMillis(): Long = System.currentTimeMillis()
      override def executionModel: ExecutionModel = ExecutionModel.SynchronousExecution
      override def withExecutionModel(em: ExecutionModel): Scheduler = ???
    }


}

