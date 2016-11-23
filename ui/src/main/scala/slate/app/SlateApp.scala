package slate
package app

import cats._
import cats.implicits._
import japgolly.scalajs.react.{ReactComponentM, ReactDOM, TopNode}
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.atnos.eff.syntax.all._
import org.atnos.eff.{Eff, Fx}
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp, JIRAApp, TodoistApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.models.{AppModel, DatedAppContent, ExpandableContentModel}
import slate.storage.{DomStorage, StorageAction, StorageFS, StorageProgram}
import slate.util.LoggerFactory
import slate.util.Util._
import slate.views.AppView.AppProps
import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Success, Try}
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import Observable.{fromTask=>toObservable}

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  sealed abstract class BootRefreshPolicy {
    def shouldRefresh(secondsAge: Int): Boolean
  }

  object BootRefreshPolicy {
    final case class IfOlderThan(seconds: Int) extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = secondsAge > seconds
    }
    case object Never extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = false
    }
    case object Always extends BootRefreshPolicy {
      override def shouldRefresh(secondsAge: Int): Boolean = true
    }
  }

  final case class SlateProgram[+P](id: Int, title: String, bootRefreshPolicy: BootRefreshPolicy, titleLink: String, program: P, input: JSON) {
    def withProgram[B](newProgram: B): SlateProgram[B] = copy(program = newProgram)
    def withoutProgram: SlateProgram[Unit] = copy(program = ())
    def nameInputHash: Int = (title, input).hashCode
  }

  object SlateProgram {

    implicit def slateProgramTraverse: Traverse[SlateProgram] = new Traverse[SlateProgram] {
      override def map[A, B](fa: SlateProgram[A])(f: (A) => B): SlateProgram[B] = fa.copy(program = f(fa.program))
      override def traverse[G[_], A, B](fa: SlateProgram[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[SlateProgram[B]] =
        f(fa.program).map(fa.withProgram)
      override def foldLeft[A, B](fa: SlateProgram[A], b: B)(f: (B, A) => B): B =
        f(b, fa.program)
      override def foldRight[A, B](fa: SlateProgram[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa.program, lb)
    }
  }

  def programs: Vector[SlateProgram[Program[FilterAST] Either String]] = {
    Vector[SlateProgram[Program[FilterAST] Either String]](
      SlateProgram(1, "Gmail", BootRefreshPolicy.IfOlderThan(seconds = 1 * 60), "https://gmail.com", GmailApp.program, JSON.ObjMap(Map())),
      SlateProgram(2, "Todoist", BootRefreshPolicy.IfOlderThan(seconds = 1 * 60), "https://todoist.com", TodoistApp.program,
        JSON.ObjMap(Map(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb")
        ))
      ),
      SlateProgram(3, "JIRA", BootRefreshPolicy.Never, "https://dashboarder.atlassian.net", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      SlateProgram(4, "Google Calendar", BootRefreshPolicy.Always, "https://calendar.google.com", GCalendarApp.program, JSON.ObjMap(Map()))
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  import shapeless.ops.coproduct.Basis

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  def prepareProgramFolder: StorageProgram[StorageFS.StorageKey[StorageFS.Dir]] = for {
    programDirKey <- StorageFS.mkDir("program", nonceSource, StorageFS.fsroot)
  } yield programDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    def reassembleProgram(program: SlateProgram[Either[Program[FilterAST], String]]): StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]] = {
      program.program.fold[StorageProgram[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]](p =>
        Eff.pure(program.withProgram(Right[ErrorGettingCachedProgram, Program[FilterAST]](p))),
        s => caching.Program.getCachedProgramByHash(program.withProgram(s)).map(program.withProgram))
    }

    val prog: StorageProgram[Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      programs.traverse(reassembleProgram)

    type StorageOrTask = Fx.fx2[StorageAction, Task]

    StorageProgram.runProgramInto(DomStorage.Local, prepareProgramFolder.into[StorageOrTask]).flatMap { programDirKey =>
      StorageFS.runSealedStorageProgramInto(prog.into[StorageOrTask], DomStorage.Local, nonceSource, programDirKey)
        .map(_.map(_.map(_.leftMap(r => Basis[ErrorCompilingPrograms, ErrorGettingCachedProgram].inverse(Right(r)))
          .flatMap {
            QQCompiler.compileProgram(SlatePrelude, _).leftMap(inj[ErrorCompilingPrograms][QQCompilationException])
          }
        )))
    }.detach
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms: Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map {
      programs =>
        programs.map {
          program =>
            program.map(
              _.map {
                compiled =>
                  CompiledFilter.run(program.input, Map.empty, compiled).map {
                    _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs)))
                  }
              }
            )
        }
    }

  case class InvalidJSON(str: String) extends Exception

  type ErrorDeserializingProgramOutput = InvalidJSON :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput: Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]]] =
    runCompiledPrograms.map {
      _.map(program =>
        program.map(
          _.map[Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]](
            _.map(
              _.leftMap(r => Basis[ErrorDeserializingProgramOutput, ErrorRunningPrograms].inverse(Right(r))).flatMap(
                _.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] {
                  json =>
                    val upickleJson = JSON.JSONToUpickleRec.apply(json)
                    try Either.right(ExpandableContentModel.pkl.read(upickleJson)) catch {
                      case ex: upickle.Invalid.Data => Either.left(inj[ErrorDeserializingProgramOutput](ex))
                    }
                }
              )
            )
          )
        )
      )
    }

  def prepareDataFolder: StorageProgram[StorageFS.StorageKey[StorageFS.Dir]] = for {
    dataDirKey <- StorageFS.mkDir("data", nonceSource, StorageFS.fsroot)
  } yield dataDirKey.get.fold(identity[StorageFS.StorageKey[StorageFS.Dir]])

  type AllErrors = InvalidJSON :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms

  val errorCompilingProgramsInAllErrors: Basis[AllErrors, ErrorCompilingPrograms] =
    Basis[AllErrors, ErrorCompilingPrograms]

  val errorDeserializingProgramOutputInAllErrors: Basis[AllErrors, ErrorDeserializingProgramOutput] =
    Basis[AllErrors, ErrorDeserializingProgramOutput]

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  def getCachedOutput(dataDirKey: StorageFS.StorageKey[StorageFS.Dir], programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[InvalidJSON Either DatedAppContent]]]] = {
    for {
      cachedContent <- StorageFS.runSealedStorageProgramInto(
        programs.traverseA(p =>
          Caching.getCachedBy[Fx.fx2[Task, StorageAction], InvalidJSON, SlateProgram[Unit], DatedAppContent](p)(
            prg => makeDataKey(prg.title, prg.input), {
              encodedModels =>
                Try(upickle.json.read(encodedModels))
                  .toOption.flatMap(DatedAppContent.pkl.read.lift)
                  .toRight(InvalidJSON(encodedModels))
            })
            .map(ms => (p, ms))), DomStorage.Local, nonceSource, dataDirKey
      ).detach
      cachedPrograms = cachedContent.map {
        case (p, models) => models.map(p.withProgram)
      }
    } yield cachedPrograms
  }

  def loadContent(dataDirKey: StorageFS.StorageKey[StorageFS.Dir]): Observable[Task[SearchPageProps]] = {
    (for {
      programOutput <- toObservable(deserializeProgramOutput)
      cachedOutputs <- toObservable(getCachedOutput(dataDirKey, programOutput.map(_.withoutProgram)).map(_.flatten))
      cachedOutputsMapped = cachedOutputs.groupBy(_.id).mapValues(_.head)
      results <- raceFold(programOutput.collect {
        case SlateProgram(id, title, bootRefreshPolicy, titleLink, out, input)
          if cachedOutputsMapped.get(id).forall(
            _.program.forall(d =>
              bootRefreshPolicy.shouldRefresh(((js.Date.now() - d.date.getTime()) / 1000.0).toInt)
            )
          ) =>
          val injectedErrors: Task[Either[AllErrors, DatedAppContent]] = out.leftMap(e =>
            errorCompilingProgramsInAllErrors.inverse(Right(e))
          ).map(_.map(_.leftMap(e => errorDeserializingProgramOutputInAllErrors.inverse(Right(e)))))
            .sequence[Task, Either[AllErrors, Vector[ExpandableContentModel]]].map((modelsErrErr: Either[AllErrors, Either[AllErrors, Vector[ExpandableContentModel]]]) =>
            modelsErrErr.flatMap(_.map(models => DatedAppContent(models.toList, new js.Date(js.Date.now()))))
          )
          injectedErrors.map(errorsOrContent =>
            errorsOrContent.traverse[Eff[Fx.fx1[Task], ?], Unit](r =>
              StorageProgram.runProgram(
                new StorageFS(DomStorage.Local, nonceSource, dataDirKey),
                StorageProgram.update(makeDataKey(title, input), DatedAppContent.pkl.write(r).toString())
              )
            ).detach.as(AppProps(id, input, title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          )
      })(
        runCompiledPrograms.map(
          _.map(t => AppProps(t.id, t.input, t.title, t.titleLink, cachedOutputsMapped.get(t.id).map {
            p =>
              p.program.leftMap(inj[AllErrors].apply[InvalidJSON]).map(pa => AppModel(pa.content, pa.date))
          })))
      )((l, ap) => Task.mapBoth(ap, l) {
        (a, li) => a +: li.filterNot(_.id == a.id)
      })
    } yield results).map(_.map(appProps => SearchPageProps(appProps.sortBy(_.id).toList)))
  }

  def render(container: org.scalajs.dom.raw.Element, content: SearchPageProps)(
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

  import scala.concurrent.duration._

  def loadAndRenderContent(dataDirKey: StorageFS.StorageKey[StorageFS.Dir], container: dom.Element)(implicit sch: Scheduler): Observable[SearchPageProps] =
    (for {
      compilePrograms <- loadContent(dataDirKey)
      outputs <- toObservable(compilePrograms)
    } yield outputs).throttleLast(500.millis).flatMap(r =>
      toObservable(render(container, r)).map(_ => r)
    )

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    val _ = (for {
      _ <- appendStyles
      dataDirKey <- StorageProgram.runProgram(DomStorage.Local, StorageFS.initFS >> prepareDataFolder).detach
      _ <- loadAndRenderContent(dataDirKey, container).completedL
    } yield ()).runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = println("QQ run loop finished successfully!")
      override def onError(ex: Throwable): Unit = println(s"QQ run loop finished with error: $ex")
    })
  }

  // TODO: cache
  private def appendStyles = Task.delay {

    import slate.views._

    val renderer = new StringRenderer.Raw(StringRenderer.formatTiny)
    val addStyles =
      Seq(
        Styles,
        ExpandableContentView.Styles,
        ErrorView.Styles,
        TitledContentView.Styles,
        AppView.Styles
      ).map(_.renderA(renderer)).mkString("\n")
    val aggregateStyles = PlatformExports.createStyleElement(addStyles)
    dom.document.head appendChild aggregateStyles
  }
}
