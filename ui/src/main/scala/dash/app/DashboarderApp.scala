package dash.app

import dash.DashboardPage.{AppBarState, SearchPageState}
import dash.{DashboardPage, LoggerFactory}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.execution.Cancelable
import monix.reactive.observers.Subscriber.Sync
import monix.reactive.{Observable, OverflowStrategy}
import org.scalajs.dom
import org.scalajs.dom.raw._

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.Any
import scala.scalajs.js.annotation.JSExport
import scalacss.{ScalaCssReactFns, StringRenderer}

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  val wheelPosition: Observable[(Double, Double)] =
    Observable.create[(Double, Double)](OverflowStrategy.Unbounded) { (s: Sync[(Double, Double)]) =>
      var posX = 0.0
      var posY = 0.0
      val wheelHandler = Any.fromFunction1((ev: WheelEvent) => {
        if (ev.deltaX == 0) {
          posX = 0
        } else {
          posX += ev.deltaX
        }
        if (ev.deltaY == 0) {
          posY = 0
        } else {
          posY += ev.deltaY
        }
        s.onNext((posX, posY))
        js.Any.fromUnit(())
      })

      dom.window.addEventListener[WheelEvent]("wheel", wheelHandler)

      Cancelable { () =>
        dom.window.removeEventListener[WheelEvent]("wheel", wheelHandler)
      }
    }.throttleLast(100.millis)

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  @JSExport
  def main(): Unit = {
    import monix.execution.Scheduler.Implicits.global
    val searchPage =
      DashboardPage
        .makeSearchPage(wheelPosition.map(p => AppBarState(p._2)))
        .build(Observable.fromTask(GMailApp.fetchMail).flatten.map(SearchPageState(_)))
    val container =
      dom.document.body.children.namedItem("container")
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Starting perf")
      Addons.Perf.start()
      logger.info("Rendering DOM")
    }
    locally {
      import dash.views._
      val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
      val aggregateStyles =
        ScalaCssReactFns.createStyle("html{\noverflow-y:scroll;\n}\n" + Styles.renderA(renderer) + "\n" +
          ExpandableContentView.Styles.renderA(renderer) + "\n" +
          TitledContentView.Styles.renderA(renderer))
      dom.document.head appendChild aggregateStyles
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
