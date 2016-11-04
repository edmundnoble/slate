package dash
package app

import dash.DashboardPage.SearchPageProps
import dash.Util._
import dash.app.ProgramCache.ErrorGettingCachedProgram
import dash.models.{AppModel, ExpandableContentModel}
import dash.views.AppView.AppProps
import japgolly.scalajs.react.{Addons, ReactComponentM, ReactDOM, TopNode}
import monix.eval.{Coeval, Task}
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.cats._
import org.scalajs.dom
import org.scalajs.dom.raw._
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{ConcreteFilter, JSON, Program}
import qq.util._
import shapeless.ops.coproduct.Unifier
import shapeless.{:+:, Inl, Inr}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.Success
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import cats.implicits._
import cats.Functor
import cats.data.Xor
import org.atnos.eff.{syntax, reader, Fx, NoFx}, syntax.all._

@JSExport
object DashboarderApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  final case class DashProgram[P](id: Int, title: String, titleLink: String, program: P, input: JSON) {
    def map[B](f: P => B): DashProgram[B] = copy(program = f(program))
    def nameInputHash: Int = (title, input).hashCode
  }

  object DashProgram {
    implicit def dashProgramFunctor: Functor[DashProgram] = new Functor[DashProgram] {
      override def map[A, B](fa: DashProgram[A])(f: (A) => B): DashProgram[B] = fa.copy(program = f(fa.program))
    }
  }

  def programs: List[DashProgram[Program[ConcreteFilter] Xor String]] = {
    val todoistState: String = List.fill(6) {
      java.lang.Integer.toHexString(scala.util.Random.nextInt(256))
    }.mkString

    List[DashProgram[Program[ConcreteFilter] Xor String]](
      DashProgram(1, "Gmail", "https://gmail.com", GmailApp.program, JSON.Obj()),
      DashProgram(2, "Todoist", "https://todoist.com", TodoistApp.program,
        JSON.Obj(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb"),
          "tok" -> JSON.Str(todoistState)
        )
      ),
      DashProgram(3, "JIRA", "https://dashboarder.atlassian.net", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      DashProgram(4, "Google Calendar", "https://calendar.google.com", GCalendarApp.program, JSON.Obj())
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  def compiledPrograms: List[DashProgram[Task[ErrorCompilingPrograms Xor CompiledFilter]]] =
    programs.map(program =>
      program.map { programPrecompiledOrString =>
        programPrecompiledOrString.fold(e => Task.now(e.right[ErrorGettingCachedProgram]), s =>
          StorageProgram.runProgram(DomStorage.Local,
            StorageProgram.retarget(ProgramCache.getCachedProgram(s))("program", " ")
          ).detach
        ).map(_.leftMap(Inr.apply).flatMap(
          QQCompiler.compileProgram(DashPrelude, _).leftMap(Inl.apply)
        ))
      }
    )

  type ErrorRunningPrograms = QQRuntimeException :+: ErrorCompilingPrograms

  private def runCompiledPrograms: List[DashProgram[Task[ErrorRunningPrograms Xor List[JSON]]]] =
    compiledPrograms.map { program =>
      program.map(
        _.map(_.leftMap(Inr[QQRuntimeException, ErrorCompilingPrograms]))
          .flatMap(_.traverse[Task, ErrorRunningPrograms, ErrorRunningPrograms Xor List[JSON]] { compiled =>
            CompiledFilter.run(program.input, Map.empty, compiled).map {
              _.leftMap(exs => Inl(QQRuntimeException(exs))).toXor
            }
          }).map(_.flatMap(identity))
      )
    }

  type ErrorDeserializingProgramOutput = upickle.Invalid.Data :+: ErrorRunningPrograms

  private def deserializeProgramOutput: List[DashProgram[Task[ErrorDeserializingProgramOutput Xor List[ExpandableContentModel]]]] =
    runCompiledPrograms.map { program =>
      program.map(
        _.map(
          _.leftMap(Inr[upickle.Invalid.Data, ErrorRunningPrograms]).flatMap(
            _.traverse[ErrorDeserializingProgramOutput Xor ?, ExpandableContentModel] {
              json =>
                val upickleJson = JSON.JSONToUpickleRec.apply(json)
                try ExpandableContentModel.pkl.read(upickleJson).right catch {
                  case ex: upickle.Invalid.Data => Inl(ex).left
                }
            }
          )
        )
      )
    }

  def getContent: Observable[SearchPageProps] =
    raceFold(deserializeProgramOutput.map {
      case DashProgram(id, title, titleLink, out, _) =>
        val errorsCaughtProgram = out.map {
          t =>
            t.swap.foreach { err =>
              logger.warn("error while running programs",
                implicitly[Unifier.Aux[ErrorDeserializingProgramOutput, Throwable]].apply(err))
            }
            AppProps(id, title, titleLink, AppModel(t))
        }
        errorsCaughtProgram
    })(
      runCompiledPrograms.map(t => AppProps(t.id, t.title, t.titleLink, AppModel(Nil.right)))
    )((l, ap) => ap :: l.filterNot(_.id == ap.id))
      .map(appProps => SearchPageProps(appProps.sortBy(_.id)))

  def render(container: Element, content: SearchPageProps)(
    implicit scheduler: Scheduler): Task[ReactComponentM[SearchPageProps, Unit, Unit, TopNode]] = {

    val searchPage =
      DashboardPage
        .makeDashboardPage
        .build(content)
    Task.create {
      (_, cb) =>
        ReactDOM.render(searchPage, container,
          js.ThisFunction.fromFunction1[ReactComponentM[SearchPageProps, Unit, Unit, TopNode], Unit](t => cb(Success(t))))
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
    val _ =
      (for {
        _ <- Observable.fromTask(appendStyles())
        props <- getContent
        _ <- Observable.fromTask(render(container, props))
      } yield ()).subscribe()
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

  // TODO: cache
  private def appendStyles() = Task.delay {
    import dash.views._
    val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
    val addStyles =
      Seq(Styles, ExpandableContentView.Styles, ErrorView.Styles, TitledContentView.Styles, AppView.Styles).map(_.renderA(renderer)).mkString("\n")
    val aggregateStyles = PlatformExports.createStyleElement(addStyles)
    dom.document.head appendChild aggregateStyles
  }
}
