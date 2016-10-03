package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageProps}
import dash.Util._
import dash.app.ProgramCache.WhatCanGoWrong
import dash.models.{AppModel, ExpandableContentModel}
import dash.views.AppView.AppProps
import japgolly.scalajs.react.{Addons, ReactComponentM, ReactDOM, TopNode}
import monix.eval.{Callback, Coeval, Task}
import monix.execution.cancelables.{CompositeCancelable, StackedCancelable}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.observers.Subscriber.Sync
import monix.reactive.{Observable, OverflowStrategy}
import monix.scalaz._
import org.scalajs.dom
import org.scalajs.dom.raw._
import qq.Platform.Rec._
import qq.cc.CompiledFilter
import qq.data.JSON
import shapeless.ops.coproduct.Unifier
import upickle.Js
import qq.util._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Failure
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.tag._
import scalaz.syntax.apply._
import scalaz.std.`try`._
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import scalaz.\/

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  def wheelPositionY: Observable[Double] =
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

  case class DashProgram[P](title: String, program: P, input: JSON)

  def programs: List[DashProgram[String]] = {
    val todoistState: String = List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString

    List(
      DashProgram[String]("Gmail", GmailApp.program, JSON.Obj()),
      DashProgram[String]("JIRA", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      DashProgram[String]("Todoist", TodoistApp.program,
        JSON.Obj(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb"),
          "tok" -> JSON.Str(todoistState)
        )
      )
    )
  }

  def compiledPrograms: List[DashProgram[Task[WhatCanGoWrong \/ CompiledFilter[JSON]]]] =
    programs.map(program =>
      program.copy(program = StorageProgram.runProgram(DomStorage.Local, ProgramCache.getCachedCompiledProgram(program.program)))
    )

  private def runCompiledPrograms: Map[String, Task[List[JSON]]] =
    compiledPrograms.map {
      case DashProgram(title, program, input) =>
        title ->
          program
            .flatMap(_.leftMap(implicitly[Unifier.Aux[WhatCanGoWrong, Throwable]].apply).valueOrThrow)
            .flatMap(_ (Map.empty)(input))
    }(collection.breakOut)

  private def deserializeProgramOutput = runCompiledPrograms.map {
    case (title, program) =>
      title ->
        program.flatMap(
            _.traverse[TaskParallel, ExpandableContentModel] { json =>
              val upickle: Js.Value = JSON.JSONToUpickleRec(json)
              Task.delay(ExpandableContentModel.pkl.read(upickle)).materialize.map(
                _.recoverWith {
                  case ex => Failure(
                    new Exception("Deserialization error, trying to deserialize " + JSON.render(json).mkString, ex)
                  )
                }
              ).dematerialize.parallel
            }.unwrap
          )
  }

  def getContent: Observable[SearchPageProps] =
    raceFold(deserializeProgramOutput.map {
      case (title, program) =>
        val errorsCaughtProgram = program.materialize.map { t =>
          t.failed.foreach {
            logger.error("error while retrieving programs", _)
          }
          AppProps(title, AppModel(toDisjunction(t)))
        }
        errorsCaughtProgram
    }(collection.breakOut))(runCompiledPrograms.map(t => t._1 -> AppProps(t._1, AppModel(Nil.right))))((l, ap) => l + (ap.title -> ap))
      .map(l => SearchPageProps(l.values.toList))

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
          ErrorView.Styles.renderA(renderer) + "\n" +
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
    getContent.foreach { props =>
      val _ = render(container, wheelPositionY, props)
    }
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
