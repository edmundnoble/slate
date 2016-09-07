package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageState}
import dash.models.{AppModel, ExpandableContentModel}
import japgolly.scalajs.react.{Addons, ReactDOM}
import monix.eval.Task
import monix.execution.Cancelable
import monix.reactive.observers.Subscriber.Sync
import monix.reactive.{Notification, Observable, OverflowStrategy}
import org.scalajs.dom
import org.scalajs.dom.raw._

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import dash.Util._
import dash.views.AppView.AppProps
import qq.jsc.Json
import monix.scalaz._
import qq.Platform.Rec._

import scala.concurrent.duration.FiniteDuration
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer

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
    }.throttleLast(new FiniteDuration(30, java.util.concurrent.TimeUnit.MILLISECONDS))

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  val programs =
    Map("Gmail" -> GmailApp.program, "JIRA" -> JIRAApp.program)

  val compiledPrograms = programs.map {
    case (t, prog) =>
      (t, StorageProgram.runProgram(DomStorage.Local,
        DashApp.getCachedCompiledProgram(prog))
        .flatMap(_.valueOrThrow)
        .flatMap(f => f(js.Object())
          .map(_.flatMap(i => ExpandableContentModel.pkl.read.lift(Json.jsToUpickleRec(i))))
        ))
  }

  def getContent: SearchPageState =
    SearchPageState(compiledPrograms.map {
      case (title, program) =>
        AppProps(title,
          Observable.fromTask(
            program.materialize.map { t =>
              t.failed.foreach {
                logger.error("error while retrieving programs", _)
              }
              t.map(AppModel(_))
            }.dematerialize
          )
        )
    }(collection.breakOut))

  def render(container: Element, wheelPosY: Observable[Double], content: SearchPageState) = {
    import dash.views._
    import monix.execution.Scheduler.Implicits.global
    val searchPage =
      DashboardPage
        .makeDashboardPage(wheelPosY.map(AppBarState(_)))
        .build(Observable.now(content))
    val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
    val aggregateStyles =
      PlatformExports.createStyleElement(
        "html{\noverflow-y:scroll;\n}\n" + Styles.renderA(renderer) + "\n" +
          ExpandableContentView.Styles.renderA(renderer) + "\n" +
          TitledContentView.Styles.renderA(renderer) + "\n" +
          AppView.Styles.renderA(renderer))
    dom.document.head appendChild aggregateStyles
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
