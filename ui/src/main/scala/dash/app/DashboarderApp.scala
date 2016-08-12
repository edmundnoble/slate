package dash.app

import dash.models.ExpandableContentModel
import dash.{DashboardPage, LoggerFactory}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.Callback
import monix.reactive.Observable
import org.scalajs.dom
import org.scalajs.dom.raw.Element

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  @JSExport
  def main(): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val searchPage = DashboardPage.makeSearchPage.build(Observable.fromTask(JIRAApp.fetchSearchResults))
    val container: Element = dom.document.body.children.namedItem("container")
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Starting perf")
      Addons.Perf.start()
      logger.info("Rendering DOM")
    }
    ReactDOM.render(searchPage, container)
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
