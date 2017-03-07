package slate
package app

import cats._
import cats.implicits._
import japgolly.scalajs.react.{ReactComponentM, ReactDOM, TopNode}
import monix.cats._
import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.scalajs.dom
import qq.Platform.Rec._
import qq.cc.{CompiledFilter, QQCompilationException, QQCompiler, QQRuntimeException}
import qq.data.{FilterAST, JSON, Program}
import shapeless.{:+:, CNil}
import slate.app.builtin.{GCalendarApp, GmailApp}
import slate.app.caching.Caching
import slate.app.caching.Program.ErrorGettingCachedProgram
import slate.models.{AppModel, DatedAppContent, ExpandableContentModel}
import slate.storage._
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
import Observable.{fromTask => toObservable}
import slate.storage.StorageFS.Dir

@JSExport
object SlateApp extends scalajs.js.JSApp {

  private[this] val logger = LoggerFactory.getLogger("DashboarderApp")

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
      SlateProgram(1, "Gmail", BootRefreshPolicy.Always, "https://gmail.com", GmailApp.program, JSON.ObjMap(Map())),
      SlateProgram(2, "Google Calendar", BootRefreshPolicy.Always, "https://calendar.google.com", GCalendarApp.program, JSON.ObjMap(Map()))
    )
  }

  type ErrorCompilingPrograms = QQCompilationException :+: ErrorGettingCachedProgram

  import shapeless.ops.coproduct.Basis

  def nonceSource(): String = {
    val int = scala.util.Random.nextInt(1000)
    int.toString
  }

  // we assume that `fsroot` exists.

  @inline def makeTopLevelFolder[F[_] : Monad, B](name: String, alreadyPresent: (Dir, StorageFS.Key[Dir]) => B, dirMade: (Dir, StorageFS.Key[Dir]) => B)
                                                 (implicit storage: Storage[F]): F[B] =
    StorageFS.mkDir[F, B](name, nonceSource, StorageFS.fsroot, alreadyPresent, dirMade).map(_.get)

  def compiledPrograms: Task[Vector[SlateProgram[Either[ErrorCompilingPrograms, CompiledFilter]]]] = {

    implicit val storage: Storage[Task] =
      DomStorage.Local

    def reassembleProgram[F[_] : Monad](program: SlateProgram[Either[Program[FilterAST], String]])(implicit storage: Storage[F]): F[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]] = {
      program.program.fold(p =>
        program.withProgram(Either.right[ErrorGettingCachedProgram, Program[FilterAST]](p)).pure[F],
        s => caching.Program.getCachedProgramByHash[F](program.withProgram(s)).map(program.withProgram))
    }

    val prog: Alg[Storage, Monad, Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
      new Alg[Storage, Monad, Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] {
        override def apply[G[_] : Monad](tg: Storage[G]): G[Vector[SlateProgram[Either[ErrorGettingCachedProgram, Program[FilterAST]]]]] =
          programs.traverse(reassembleProgram[G](_)(Monad[G], tg))
      }

    for {
      programDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("program", (_, k) => k, (_, k) => k)
      cachedPrograms <- StorageFS.runSealedStorageProgram(
        prog,
        DomStorage.Local,
        CompressedStorage[Task],
        nonceSource,
        programDirKey
      )
      errorHandledContent = cachedPrograms.map(_.map(
        _.leftMap(r => Basis[ErrorCompilingPrograms, ErrorGettingCachedProgram].inverse(Right(r)))
          .flatMap {
            QQCompiler.compileProgram(SlatePrelude, _).leftMap(inj[ErrorCompilingPrograms][QQCompilationException])
          }
      ))
    } yield errorHandledContent

  }

  type ErrorRunningPrograms = QQRuntimeException :+: CNil

  private def runCompiledPrograms: Task[Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]] =
    compiledPrograms.map {
      _.map {
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
  deserializeProgramOutput(programs: Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorRunningPrograms Either Vector[JSON]]]]
                          ): Vector[SlateProgram[ErrorCompilingPrograms Either Task[ErrorDeserializingProgramOutput Either Vector[ExpandableContentModel]]]] =
    programs.map {
      _.map(_.map(_.map(
        _.leftMap(r => Basis[ErrorDeserializingProgramOutput, ErrorRunningPrograms].inverse(Right(r)))
          .flatMap(_.traverse[ErrorDeserializingProgramOutput Either ?, ExpandableContentModel] { json =>
            val upickleJson = JSON.JSONToUpickleRec.apply(json)
            try {
              Either.right(ExpandableContentModel.pkl.read(upickleJson))
            } catch {
              case ex: upickle.Invalid.Data => Either.left(inj[ErrorDeserializingProgramOutput](ex))
            }
          })
      )))
    }


  type AllErrors = InvalidJSON :+: upickle.Invalid.Data :+: QQRuntimeException :+: ErrorCompilingPrograms

  val errorCompilingProgramsInAllErrors: Basis[AllErrors, ErrorCompilingPrograms] =
    Basis[AllErrors, ErrorCompilingPrograms]

  val errorDeserializingProgramOutputInAllErrors: Basis[AllErrors, ErrorDeserializingProgramOutput] =
    Basis[AllErrors, ErrorDeserializingProgramOutput]

  def makeDataKey(title: String, input: JSON): String =
    title + "|" + input.hashCode()

  def getCachedOutput(dataDirKey: StorageFS.DirKey, programs: Vector[SlateProgram[Unit]]): Task[Vector[Option[SlateProgram[InvalidJSON Either DatedAppContent]]]] = {
    for {
      cachedContent <-
      StorageFS.runSealedStorageProgram(
        new Alg[Storage, Monad, Vector[(SlateProgram[Unit], Option[Either[InvalidJSON, DatedAppContent]])]] {
          override def apply[G[_] : Monad](tg: Storage[G]): G[Vector[(SlateProgram[Unit], Option[Either[InvalidJSON, DatedAppContent]])]] =
            programs.traverse(p =>
              Caching.getCachedBy[G, InvalidJSON, SlateProgram[Unit], DatedAppContent](p)(
                prg => makeDataKey(prg.title, prg.input), {
                  encodedModels =>
                    Try(upickle.json.read(encodedModels))
                      .toOption.flatMap(DatedAppContent.pkl.read.lift)
                      .toRight(InvalidJSON(encodedModels))
                }
              )(Monad[G], tg).map(ms => (p, ms))
            )
        },
        DomStorage.Local, CompressedStorage[Task], nonceSource, dataDirKey
      )
      cachedPrograms = cachedContent.map {
        case (p, models) => models.map(p.withProgram)
      }
    } yield cachedPrograms
  }

  def loadContent(dataDirKey: StorageFS.DirKey): Observable[Task[SearchPageProps]] = {
    (for {
      programOutput <- toObservable(runCompiledPrograms.map(deserializeProgramOutput))
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
          val storage =
            CompressedStorage(StorageFS(DomStorage.Local, nonceSource, dataDirKey))
          val cacheContent = injectedErrors.map(errorsOrContent =>
            errorsOrContent.traverse[Task, AllErrors, Unit](r =>
              storage.update(makeDataKey(title, input), DatedAppContent.pkl.write(r).toString())
            ).map(_ => AppProps(id, input, title, titleLink, Some(errorsOrContent.map(o => AppModel(o.content, o.date)))))
          )
          cacheContent
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

  @JSExport
  def main(): Unit = {

    import monix.execution.Scheduler.Implicits.global

    val container: dom.Element =
      dom.document.body.children.namedItem("container")
    implicit val storage: DomStorage = DomStorage.Local
    val loadingThrottle: FiniteDuration = 1000.millis
    val run: Task[Unit] =
      for {
        _ <- appendStyles
        root <- StorageFS.getRoot[Task]
        _ <- StorageFS.checkFS[Task](root)
        dataDirKey <- makeTopLevelFolder[Task, StorageFS.DirKey]("data", (_, k) => k, (_, k) => k)
        _ <- loadContent(dataDirKey)
          .flatMap(toObservable)
          .throttleLast(loadingThrottle)
          .flatMap(r =>
            toObservable(render(container, r)).map(_ => r)
          )
          .completedL
      } yield ()
    val _ = run.runAsync(new monix.eval.Callback[Unit] {
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
