package dash

import dash.models.ExpandableContentModel
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.Callback
import monix.reactive.Observable
import monocle.function.At
import org.scalajs.dom.raw.Element

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.ext.LocalStorage

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  @JSExport
  def main(): Unit = {
    import monix.execution.Scheduler.Implicits.global

    val callback = new Callback[Observable[IndexedSeq[ExpandableContentModel]]] {
      override def onSuccess(results: Observable[IndexedSeq[ExpandableContentModel]]): Unit = {
        val searchPage =
          DashboardPage.makeSearchPage(results)
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

      override def onError(error: Throwable): Unit = {
        logger.error(s"EXCEPTION INTERRUPTED MAIN:\n$error\n${error.getStackTrace.mkString("\n")}")
      }
    }

    val _ = JIRAApp.fetchSearchResults.runAsync(callback)
    println(s"value: ${dom.window.localStorage.getItem("testKey")}")
  }

}
