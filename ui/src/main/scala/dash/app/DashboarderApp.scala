package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageProps}
import dash.Util._
import dash.app.ProgramCache.WhatCanGoWrong
import dash.models.{AppModel, ExpandableContentModel}
import dash.views.AppView.AppProps
import japgolly.scalajs.react.{Addons, ReactComponentM, ReactDOM, TopNode}
import monix.eval.Task
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
import qq.util._

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.tag._
import scalaz.syntax.apply._
import scalaz.syntax.std.`try`._
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

  case class DashProgram[P](id: Int, title: String, titleLink: String, program: P, input: JSON)

  def programs: List[DashProgram[String]] = {
    val todoistState: String = List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString

    List(
      DashProgram[String](1, "Gmail", "https://gmail.com", GmailApp.program, JSON.Obj()),
      DashProgram[String](2, "Todoist", "https://todoist.com", TodoistApp.program,
        JSON.Obj(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb"),
          "tok" -> JSON.Str(todoistState)
        )
      ),
      DashProgram[String](3, "JIRA", "https://dashboarder.atlassian.net", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      )
    )
  }

  def compiledPrograms: List[DashProgram[Task[WhatCanGoWrong \/ CompiledFilter[JSON]]]] =
    programs.map(program =>
      program.copy(program = StorageProgram.runProgram(DomStorage.Local, ProgramCache.getCachedCompiledProgram(program.program)))
    )

  private def runCompiledPrograms: List[(Int, String, String, Task[List[JSON]])] =
    compiledPrograms.map {
      case DashProgram(id, title, titleLink, program, input) =>
        (id, title, titleLink,
          program
            .flatMap(_.leftMap(implicitly[Unifier.Aux[WhatCanGoWrong, Throwable]].apply).valueOrThrow)
            .flatMap(_ (Map.empty)(input)))
    }(collection.breakOut)

  private def deserializeProgramOutput: List[(Int, String, String, Task[List[ExpandableContentModel]])] = runCompiledPrograms.map {
    case (id, title, titleLink, program) =>
      (id, title, titleLink,
        program.flatMap(
          _.traverse[TaskParallel, ExpandableContentModel] { json =>
            val upickle = JSON.JSONToUpickleRec.apply(json)
            Task.delay(ExpandableContentModel.pkl.read(upickle)).materialize.map(
              _.recoverWith {
                case ex => Failure(
                  new Exception("Deserialization error, trying to deserialize " + JSON.render(json), ex)
                )
              }
            ).dematerialize.parallel
          }.unwrap
        ))
  }

  def getContent: Observable[SearchPageProps] =
    raceFold(deserializeProgramOutput.map {
      case (id, title, titleLink, program) =>
        val errorsCaughtProgram = program.materialize.map { t =>
          t.failed.foreach {
            logger.error("error while retrieving programs", _)
          }
          AppProps(id, title, titleLink, AppModel(t.toDisjunction))
        }
        errorsCaughtProgram
    }(collection.breakOut))(
      runCompiledPrograms.map(t => AppProps(t._1, t._2, t._3, AppModel(Nil.right))).sortBy(_.title)
    )((l, ap) => (ap :: l.filterNot(_.title == ap.title)).sortBy(_.title))
      .map(SearchPageProps(_))

  def render(container: Element, wheelPosY: Observable[Double], content: SearchPageProps)(
    implicit scheduler: Scheduler): Task[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] = {
    import dash.views._
    val searchPage =
      DashboardPage
        .makeDashboardPage(wheelPosY.map(AppBarState(_)))
        .build(content)
    val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
    val addStyles =
      Seq(Styles, ExpandableContentView.Styles, ErrorView.Styles, TitledContentView.Styles, AppView.Styles).map(_.renderA(renderer)).mkString("\n")
    val aggregateStyles = PlatformExports.createStyleElement("html{\noverflow-y:scroll;\n}\n" + addStyles)
    dom.document.head appendChild aggregateStyles
    Task.create[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] { (sch, cb) =>
      ReactDOM.render(searchPage, container,
        js.ThisFunction.fromFunction1((t: ReactComponentM[SearchPageProps, Unit, Unit, TopNode]) => cb.apply(Success(t))))
      Cancelable.empty
    }
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
    val _ = getContent.flatMap { props =>
      Observable.fromTask(render(container, wheelPositionY, props))
    }.subscribe()
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
