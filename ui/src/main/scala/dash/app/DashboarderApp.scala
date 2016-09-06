package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageState}
import dash.models.ExpandableContentModel
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.Task
import monix.execution.Cancelable
import monix.reactive.observers.Subscriber.Sync
import monix.reactive.{Notification, Observable, OverflowStrategy}
import org.scalajs.dom
import org.scalajs.dom.raw._

import scala.concurrent.duration._
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalacss.{ScalaCssReactFns, StringRenderer}
import dash.Util._
import qq.jsc.Json
import monix.scalaz._
import qq.Platform.Rec._

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  val wheelPositionY: Observable[Double] =
    Observable.create[Double](OverflowStrategy.Unbounded) { (s: Sync[Double]) =>
      var posY = 0.0
      val wheelHandler = js.Any.fromFunction1((ev: WheelEvent) => {
        if (ev.deltaY == 0) {
          posY = 0
        } else {
          posY += ev.deltaY
        }
        s.onNext(posY)
        js.Any.fromUnit(())
      })

      dom.window.addEventListener[WheelEvent]("wheel", wheelHandler)

      Cancelable { () =>
        dom.window.removeEventListener[WheelEvent]("wheel", wheelHandler)
      }
    }.throttleLast(30.millis)

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  val programs =
    List(GmailApp.program, JIRAApp.program)

  val compiledPrograms = programs.map(prog =>
    StorageProgram.runProgram(DomStorage.Local,
      DashApp.getCachedCompiledProgram(prog))
      .flatMap(_.valueOrThrow)
      .flatMap(f => f(js.Object())
        .map(_.flatMap(i => ExpandableContentModel.pkl.read.lift(Json.jsToUpickleRec(i))))
      )
  )

  def getContent =
    Observable.fromTask(Task.gatherUnordered(compiledPrograms))
      .map(_.flatten.toIndexedSeq)
      .materialize
      .map { t =>
        t match {
          case Notification.OnError(ex) =>
            logger.error("error while rendering", ex)
          case _ =>
        }
        t
      }.dematerialize

  def render(container: Element, wheelPosY: Observable[Double], content: Observable[IndexedSeq[ExpandableContentModel]]) = {
    import monix.execution.Scheduler.Implicits.global
    val searchPage =
      DashboardPage
        .makeSearchPage(wheelPosY.map(AppBarState(_)))
        .build(content.map(f => SearchPageState(f.toList)))
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
  }

  @JSExport
  def main(): Unit = {
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Starting perf")
      Addons.Perf.start()
      logger.info("Rendering DOM")
    }
    val container =
      dom.document.body.children.namedItem("container")
    render(container, wheelPositionY, getContent)
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
