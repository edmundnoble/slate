package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageProps}
import dash.Util._
import dash.app.ProgramCache.WhatCanGoWrong
import dash.models.{AppModel, ExpandableContentModel}
import dash.views.AppView.AppProps
import japgolly.scalajs.react.{Addons, ReactComponentM, ReactDOM, TopNode}
import monix.eval.{Coeval, Task}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.observers.Subscriber.Sync
import monix.reactive.{Observable, OverflowStrategy}
import monix.scalaz._
import org.scalajs.dom
import org.scalajs.dom.raw._
import qq.Platform.Rec._
import qq.data.JSON
import shapeless.ops.coproduct.Unifier
import upickle.Js

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Failure
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.std.`try`._
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import scalaz.\/

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

  case class DashProgram(title: String, program: String, input: JSON)

  val programs = {
    val todoistState: String = List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString

    List(
      DashProgram("Gmail", GmailApp.program, JSON.Obj()),
      DashProgram("JIRA", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      DashProgram("Todoist", TodoistApp.program,
        JSON.Obj(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "tok" -> JSON.Str(todoistState)
        )
      )
    )
  }

  private def runCompiledPrograms: List[(String, Task[List[ExpandableContentModel]])] =
    programs.map {
      case DashProgram(title, program, input) =>
        (title,
          StorageProgram.runProgram(DomStorage.Local,
            ProgramCache.getCachedCompiledProgram(program))
            .flatMap(coe => coe.leftMap(implicitly[Unifier.Aux[WhatCanGoWrong, Throwable]].apply).valueOrThrow)
            .flatMap(f => f(Map.empty)(input)
              .flatMap(_.traverse { i =>
                val upickle: Js.Value = JSON.JSONToUpickleRec(i)
                Task.coeval(Coeval.delay(ExpandableContentModel.pkl.read(upickle)).materialize.map {
                  case Failure(ex) => Failure(new Exception(s"Deserialization error, trying to deserialize ${JSON.render(i).mkString}", ex))
                  case s => s
                }.dematerialize)
              })
            ))
    }

  def getContent: Task[SearchPageProps] =
    runCompiledPrograms.traverse[Task, AppProps] {
      case (title, program) =>
        program.materialize.map { t =>

          t.failed.foreach {
            logger.error("error while retrieving programs", _)
          }

          AppProps(title, AppModel(toDisjunction(t)))
        }
    }.map(SearchPageProps(_))

  def render(container: Element, wheelPosY: Observable[Double], content: SearchPageProps)(implicit scheduler: Scheduler): ReactComponentM[SearchPageProps, Unit, Unit, TopNode] = {
    import dash.views._
    val searchPage =
      DashboardPage
        .makeDashboardPage(wheelPosY.map(AppBarState(_)))
        .build(content)
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
    import monix.execution.Scheduler.Implicits.global
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Starting perf")
      Addons.Perf.start()
      logger.info("Rendering DOM")
    }
    val container =
      dom.document.body.children.namedItem("container")
    val initialState = SearchPageProps(runCompiledPrograms.map(_._1).map(AppProps(_, AppModel(Nil.right))))
    render(container, wheelPositionY, initialState)
    getContent.runAsync.foreach(render(container, wheelPositionY, _))
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
