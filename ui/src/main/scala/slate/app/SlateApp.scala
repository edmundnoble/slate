package slate
package app

import slate.views.DashboardPage
import slate.views.DashboardPage.SearchPageProps
import slate.util.Util._
import slate.app.ProgramCache.ErrorGettingCachedProgram
import slate.models.{AppModel, ExpandableContentModel}
import slate.util.LoggerFactory
import slate.storage.{DomStorage, StorageAction, StorageFS, StorageProgram}
import slate.views.AppView.AppProps
import japgolly.scalajs.react.{Addons, ReactComponentM, ReactDOM, TopNode}
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import monix.cats._
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{ConcreteFilter, JSON, Program}
import qq.util._
import shapeless.ops.coproduct.Unifier
import shapeless.{:+:, CNil, Inl, Inr}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.util.{Success, Try}
import scalacss.defaults.PlatformExports
import scalacss.internal.StringRenderer
import cats._
import cats.data._
import cats.implicits._
import monix.execution.cancelables.StackedCancelable
import org.atnos.eff.{Eff, Fx, Member, NoFx, writer}
import org.atnos.eff.syntax.all._
import upickle.Invalid.Data

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

  final class EmptyResponseException extends java.lang.Exception("Empty response")

  final case class DashProgram[+P](id: Int, title: String, titleLink: String, program: P, input: JSON) {
    def withProgram[B](newProgram: B): DashProgram[B] = copy(program = newProgram)
    def withoutProgram: DashProgram[Unit] = copy(program = ())
    def nameInputHash: Int = (title, input).hashCode
  }

  object DashProgram {

    implicit def dashProgramTraverse: Traverse[DashProgram] = new Traverse[DashProgram] {
      override def map[A, B](fa: DashProgram[A])(f: (A) => B): DashProgram[B] = fa.copy(program = f(fa.program))
      override def traverse[G[_], A, B](fa: DashProgram[A])(f: (A) => G[B])(implicit evidence$1: Applicative[G]): G[DashProgram[B]] =
        f(fa.program).map(fa.withProgram)
      override def foldLeft[A, B](fa: DashProgram[A], b: B)(f: (B, A) => B): B =
        f(b, fa.program)
      override def foldRight[A, B](fa: DashProgram[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        f(fa.program, lb)
    }
  }

  def programs: List[DashProgram[Program[ConcreteFilter] Either String]] = {
    List[DashProgram[Program[ConcreteFilter] Either String]](
      DashProgram(1, "Gmail", "https://gmail.com", GmailApp.program, JSON.ObjMap(Map())),
      DashProgram(2, "Todoist", "https://todoist.com", TodoistApp.program,
        JSON.ObjMap(Map(
          "client_id" -> JSON.Str(Creds.todoistClientId),
          "client_secret" -> JSON.Str(Creds.todoistClientSecret),
          "redirect_uri" -> JSON.Str("https://ldhbkcmhfmoaepapkcopmigahjdiekil.chromiumapp.org/provider_cb")
        ))
      ),
      DashProgram(3, "JIRA", "https://dashboarder.atlassian.net", JIRAApp.program,
        JSON.Obj(
          "username" -> JSON.Str(Creds.jiraUsername),
          "password" -> JSON.Str(Creds.jiraPassword)
        )
      ),
      DashProgram(4, "Google Calendar", "https://calendar.google.com", GCalendarApp.program, JSON.ObjMap(Map()))
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

  def compiledPrograms: Task[List[DashProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    def reassembleProgram(program: DashProgram[Either[Program[ConcreteFilter], String]]): StorageProgram[DashProgram[Either[ErrorGettingCachedProgram, Program[ConcreteFilter]]]] = {
      program.program.fold[StorageProgram[DashProgram[Either[ErrorGettingCachedProgram, Program[ConcreteFilter]]]]](p =>
        Eff.pure(program.withProgram(Right[ErrorGettingCachedProgram, Program[ConcreteFilter]](p))),
        s => ProgramCache.getCachedProgramByHash(program.withProgram(s)).map(program.withProgram))
    }

    val prog: StorageProgram[List[DashProgram[Either[ErrorGettingCachedProgram, Program[ConcreteFilter]]]]] =
      programs.traverse(reassembleProgram)

    type StorageOrTask = Fx.fx2[StorageAction, Task]

    StorageProgram.runProgramInto(DomStorage.Local, prepareProgramFolder.into[StorageOrTask]).flatMap { programDirKey =>
      StorageFS.runSealedStorageProgramInto(prog.into[StorageOrTask], DomStorage.Local, nonceSource, programDirKey)
        .map(_.map(_.map(_.leftMap(r => Basis[ErrorCompilingPrograms, ErrorGettingCachedProgram].inverse(Right(r)))
          .flatMap { p =>
            QQCompiler.compileProgram(DashPrelude, p).leftMap(inj[ErrorCompilingPrograms].apply[QQCompilationException])
          }
        )))
    }.detach
  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms: Task[List[DashProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either List[JSON]]]]] =
    compiledPrograms.map {
      programs =>
        programs.map {
          program =>
            program.map(
              _.map {
                compiled =>
                  CompiledFilter.run(program.input, Map.empty, compiled).map {
                    _.leftMap[ErrorRunningPrograms](exs => inl(QQRuntimeException(exs))).toEither
                  }
              }
            )
        }
    }

  case class InvalidJSON(str: String) extends Exception

  type ErrorDeserializingProgramOutput = InvalidJSON :+: upickle.Invalid.Data :+: ErrorRunningPrograms

  private def
  deserializeProgramOutput: Task[List[DashProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either List[ExpandableContentModel]]]]] =
    runCompiledPrograms.map {
      _.map(program =>
        program.map(
          _.map[Task[ErrorDeserializingProgramOutput Either List[ExpandableContentModel]]](
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

  def makeDataKey(title: String, input: JSON) = title + " " + input.hashCode()

  def getCachedOutput(dataDirKey: StorageFS.StorageKey[StorageFS.Dir], programs: List[DashProgram[Unit]]): Task[List[Option[DashProgram[InvalidJSON Either List[ExpandableContentModel]]]]] = {
    type mem1 = Member.Aux[StorageAction, Fx.fx2[Task, StorageAction], Fx.fx1[Task]]
    type mem2 = Member[Task, Fx.fx1[Task]]
    for {
      progs <- deserializeProgramOutput
      progMap = progs.groupBy(p => makeDataKey(p.title, p.input))
      porgs <- StorageFS.runSealedStorageProgramInto[Fx.fx2[Task, StorageAction], Fx.fx1[Task], Task, List[(DashProgram[Unit], Option[Either[InvalidJSON, List[ExpandableContentModel]]])]](
        programs.traverseA(p =>
          ProgramCache.getCachedBy[Fx.fx2[Task, StorageAction], InvalidJSON, DashProgram[Unit], List[ExpandableContentModel]](p)(
            prg => makeDataKey(prg.title, prg.input), {
              encodedModels =>
                Try(upickle.json.read(encodedModels).arr.toList)
                  .toOption.flatMap(_.traverse(ExpandableContentModel.pkl.read.lift))
                  .toRight(InvalidJSON(encodedModels))
            })
            .map(ms => (p, ms))), DomStorage.Local, nonceSource, dataDirKey)(Monad[Task], implicitly[mem1], implicitly[mem2]).detach
      cachedPrograms = porgs.map {
        case (p, models) => models.map(p.withProgram)
      }
    } yield cachedPrograms
  }

  def getContent(dataDirKey: StorageFS.StorageKey[StorageFS.Dir]): Observable[Task[SearchPageProps]] = {
    (for {
      programOutput <- Observable.fromTask(deserializeProgramOutput)
      cachedOutputs <- Observable.fromTask(getCachedOutput(dataDirKey, programOutput.map(_.withoutProgram)).map(_.flatten))
      cachedOutputsMapped = cachedOutputs.groupBy(_.id).mapValues(_.head).collect { case (k, DashProgram(_, _, _, Right(li), _)) => (k, li) }
      results <- raceFold(programOutput.map {
        case DashProgram(id, title, titleLink, out, input) =>
          val injectedErrors = out.leftMap(e =>
            errorCompilingProgramsInAllErrors.inverse(Right(e))
          ).map(_.map(_.leftMap(e => errorDeserializingProgramOutputInAllErrors.inverse(Right(e)))))
            .sequence[Task, Either[AllErrors, List[ExpandableContentModel]]].map(_.flatten)
          injectedErrors.map(e => Task.now(AppProps(id, input, title, titleLink, AppModel(e))))
      })(
        runCompiledPrograms.map(
          _.map(t => AppProps(t.id, t.input, t.title, t.titleLink, AppModel(Either.right(cachedOutputsMapped.getOrElse(t.id, Nil)))))
        )
      )((l, ap) => Task.mapBoth(ap, l) {
        (a, li) => a :: li.filterNot(_.id == a.id)
      })
    } yield results)
      .map(_.map(appProps => SearchPageProps(appProps.sortBy(_.id))))
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

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global
    import Observable.fromTask

    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Starting perf")
      Addons.Perf.start()
      logger.info("Rendering DOM")
    }
    val container =
      dom.document.body.children.namedItem("container")
    val _ = (for {
      dataDirKey <-
      StorageProgram.runProgram[Task, Fx.fx1[Task], Fx.fx1[StorageAction], NoFx, StorageFS.StorageKey[StorageFS.Dir]](DomStorage.Local, StorageFS.initFS >> prepareDataFolder).detach
      lastResults <- Task.fromFuture((for {
        _ <- fromTask(appendStyles())
        compilePrograms <- getContent(dataDirKey)
        outputs <- fromTask(compilePrograms)
        _ <- fromTask(render(container, outputs))
      } yield outputs).runAsyncGetLast)
      fs = new StorageFS(DomStorage.Local, nonceSource, dataDirKey)
      _ <- lastResults.traverse(r => StorageProgram.runProgram(fs, r.appProps.traverse {
        case AppProps(_, input, title, _, AppModel(content)) =>
          Eff.traverseA(content)(cs => StorageProgram.update(makeDataKey(title, input), ExpandableContentModel.pkls.write(cs).toString()))
      }).detach) // StorageProgram.update)
    } yield ()).runAsync(new monix.eval.Callback[Unit] {
      override def onSuccess(value: Unit): Unit = println("QQ run loop finished successfully!")
      override def onError(ex: Throwable): Unit = println(s"QQ run loop finished with error: $ex")
    })
    if (!js.isUndefined(Addons.Perf)) {
      logger.info("Stopping perf")
      Addons.Perf.stop()
      logger.info("Printing wasted")
      logger.info(Addons.Perf.printWasted(Addons.Perf.getLastMeasurements()).toString)
    }
  }

  // TODO: cache
  private def appendStyles() = Task.delay {

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
