package dash
package app

import dash.DashboardPage.{AppBarState, SearchPageProps}
import dash.Util._
import dash.app.ProgramCache.ErrorGettingCachedProgram
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
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeError, QQRuntimeException}
import qq.data.{ConcreteFilter, JSON, Program}
import shapeless.ops.coproduct.Unifier
import qq.util._
import shapeless.{:+:, Inl, Inr}
import upickle.Invalid.Data

import scala.concurrent.duration.FiniteDuration
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Failure, Success}
import scalaz.syntax.traverse._
import scalaz.syntax.either._
import scalaz.std.list._
import scalaz.syntax.tag._
import scalaz.syntax.apply._
import scalaz.syntax.bind._
import scalaz.syntax.std.`try`._
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import scalaz.{Functor, ValidationNel, \/}

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

  case class DashProgram[P](id: Int, title: String, titleLink: String, program: P, input: JSON) {
    def map[B](f: P => B): DashProgram[B] = copy(program = f(program))
  }

  object DashProgram {
    implicit def dashProgramFunctor: Functor[DashProgram] = new Functor[DashProgram] {
      override def map[A, B](fa: DashProgram[A])(f: (A) => B): DashProgram[B] = fa.copy(program = f(fa.program))
    }
  }

  def programs: List[DashProgram[Program[ConcreteFilter] \/ String]] = {
    val todoistState: String = List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString

    List[DashProgram[Program[ConcreteFilter] \/ String]](
      DashProgram(1, "Gmail", "https://gmail.com", GmailApp.program.left, JSON.Obj()),
      DashProgram(2, "Todoist", "https://todoist.com", TodoistApp.program.left,
        JSON.Obj(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb"),
          "tok" -> JSON.Str(todoistState)
        )
      ),
      DashProgram(3, "JIRA", "https://dashboarder.atlassian.net", JIRAApp.program.left,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      DashProgram(4, "Google Calendar", "https://calendar.google.com", GCalendarApp.program.left, JSON.Obj())
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  def compiledPrograms: List[DashProgram[Task[ErrorCompilingPrograms \/ CompiledFilter]]] =
    programs.map(program =>
      program.map { programPrecompiledOrString =>
        programPrecompiledOrString.fold(e => Task.now(e.right[ErrorGettingCachedProgram]), s =>
          StorageProgram.runRetargetableProgram(DomStorage.Local, "program", ProgramCache.getCachedProgram(s))
        ).map(_.leftMap(Inr[QQCompilationException, ErrorGettingCachedProgram]).flatMap(
          QQCompiler.compileProgram(DashPrelude, _).leftMap(Inl[QQCompilationException, ErrorGettingCachedProgram])
        ))
      }
    )

  type ErrorRunningPrograms = QQRuntimeException :+: ErrorCompilingPrograms

  private def runCompiledPrograms: List[(Int, String, String, Task[ErrorRunningPrograms \/ List[JSON]])] =
    compiledPrograms.map {
      case DashProgram(id, title, titleLink, program, input) =>
        (id, title, titleLink,
          program
            .map(_.leftMap(Inr[QQRuntimeException, ErrorCompilingPrograms]))
            .flatMap(_.traverse[Task, ErrorRunningPrograms, ErrorRunningPrograms \/ List[JSON]] { f =>
              val r = f(Map.empty)(input).materialize.map { e =>
                e.get.leftMap(exs => Inl[QQRuntimeException, ErrorCompilingPrograms](QQRuntimeException(exs))).disjunction
              }
              r
            }).map(_.flatMap(identity))
        )
    }

  type ErrorDeserializingProgramOutput = upickle.Invalid.Data :+: ErrorRunningPrograms

  private def deserializeProgramOutput: List[(Int, String, String, Task[ErrorDeserializingProgramOutput \/ List[ExpandableContentModel]])] = runCompiledPrograms.map {
    case (id, title, titleLink, program) =>
      (id, title, titleLink, {
        val z: Task[ErrorDeserializingProgramOutput \/ List[ExpandableContentModel]] =
          program.map {
            (y: ErrorRunningPrograms \/ List[JSON]) =>
              val x = y.leftMap(Inr[upickle.Invalid.Data, ErrorRunningPrograms]).flatMap {
                jsons =>
                  jsons.traverse[ErrorDeserializingProgramOutput \/ ?, ExpandableContentModel] {
                    json =>
                      val upickleJson = JSON.JSONToUpickleRec.apply(json)
                      Coeval.delay(ExpandableContentModel.pkl.read(upickleJson).right).onErrorRecover {
                        case ex: upickle.Invalid.Data => Inl[upickle.Invalid.Data, ErrorRunningPrograms](ex).left
                      }.value
                  }
              }
              x
          }
        z
      })
  }

  def getContent: Observable[SearchPageProps] =
    raceFold(deserializeProgramOutput.map {
      case (id, title, titleLink, program) =>
        val errorsCaughtProgram = program.map {
          t =>
            t.swap
              .map(implicitly[Unifier.Aux[ErrorDeserializingProgramOutput, Throwable]].apply)
              .foreach {
                logger.warn("error while running programs", _)
              }
            AppProps(id, title, titleLink, AppModel(t))
        }
        errorsCaughtProgram
    }(collection.breakOut))(
      runCompiledPrograms.map(t => AppProps(t._1, t._2, t._3, AppModel(Nil.right)))
    )((l, ap) => ap :: l.filterNot(_.title == ap.title))
      .map(appProps => SearchPageProps(appProps.sortBy(_.title)))

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
    val aggregateStyles = PlatformExports.createStyleElement(addStyles)
    dom.document.head appendChild aggregateStyles
    Task.create {
      (_, cb) =>
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
    val _ = getContent.flatMap {
      props =>
        Observable.fromTask(render(container, Observable.never, props))
    }.subscribe()
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

}
